!If you are trying to find where Clarion is installed look in the registy
!   HKLM\SOFTWARE\SoftVelocity\Clarion##  ROOT value where ##=11 10 9.1 9 8 7
!Function GetClarionInstalls() returns Q with Install Root Paths, Clarion##, ##
!
  PROGRAM
  MAP
Test_GetClarionInstalls PROCEDURE() 
GetClarionInstalls      PROCEDURE(QUEUE ClarionQ, *STRING ClaQ_RootPath, <*STRING ClaQ_ClarionName>, <*DECIMAL ClaQ_VersioNo>)
!^^^^^^ Clarion Installs added to Queue                   C:\Clarion              Clarion11                    11

ClarionInstallsFromRegistryExplorer PROCEDURE()   !
  END

  CODE
  Test_GetClarionInstalls()

!================================================================
Test_GetClarionInstalls PROCEDURE()

ClarionInstallQ QUEUE,PRE(ClaInstQ) 
VersionNo DECIMAL(5,1)  ! ClaInstQ:VersionNo E.g. 11 Number for sorting to find newest  (highest)
Clarion   STRING(16)    ! ClaInstQ:Clarion   E.g. Clarion11 ... Clarion9.1
Root      STRING(128)   ! ClaInstQ:Root      E.g. C:\Clarion11
          END
          
Window WINDOW('Test GetClarionInstalls() Function'),AT(,,345,139),GRAY,SYSTEM,FONT('Segoe UI',10,,FONT:regular)
        STRING('Call: GetClarionInstalls(ClaQ, ClaQ.RootPath, ClaQ.Clarion, ClQ.VersionNo)'),AT(7,7),USE(?HowDone), |
                FONT('Consolas')
        LIST,AT(8,24,327,82),USE(?LIST:Clarion2Q),VSCROLL,FROM(ClarionInstallQ),FORMAT('33R(7)|M~Verison~C(0)@n4.1@70L(2' & |
                ')|M~Clarion  (SubKey)~@s16@120L(2)|M~Root Path  (HKLM,SOFTWARE\SoftVelocity\Clarion#,ROOT)~@s128@')
        BUTTON('&Explore More'),AT(89,114,60),USE(?ExploreBtn)
        BUTTON('Cl&ose'),AT(196,114,60),USE(?Close),STD(STD:Close)
    END
    CODE 
    GetClarionInstalls(ClarionInstallQ, ClaInstQ:Root, ClaInstQ:Clarion, ClaInstQ:VersionNo)
    SORT(ClarionInstallQ, -ClaInstQ:VersionNo, -ClaInstQ:Clarion, -ClaInstQ:Root )
    OPEN(Window)
    ACCEPT
        CASE ACCEPTED()
        OF ?ExploreBtn ; START(ClarionInstallsFromRegistryExplorer)
        END
    END    
!==================================================================================== 
!====================================================================================    
GetClarionInstalls      PROCEDURE(QUEUE Cla_Queue, *STRING ClaQ_RootPath,<*STRING ClaQ_ClarionName>,<*DECIMAL ClaQ_VersioNo>)
SVSubKeysQ  QUEUE,PRE(SVSubQ)      !Keys under 'SOFTWARE\SoftVelocity'
ClarionKey      STRING(16)         !Want Clarion10 Clarion9.1 etc
            END
Ndx  LONG,AUTO
Root STRING(256)
SOFTWARE_SoftVelocity EQUATE('SOFTWARE\SoftVelocity')
    CODE
    GetRegSubKeys(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity, SVSubKeysQ) 
    LOOP Ndx=1 TO RECORDS(SVSubKeysQ)
        GET(SVSubKeysQ,Ndx)
        IF lower(SvSubQ:ClarionKey[1:7])<>'clarion' THEN CYCLE.  !Only take 'Clarion', SV could add other products.
        Root = GETREG(REG_LOCAL_MACHINE, SOFTWARE_SoftVelocity &'\'& SvSubQ:ClarionKey  ,'ROOT')
        IF ~Root         |                  !Blank indicates Root Value NOT found in Registry
        OR ~EXISTS(Root) THEN CYCLE.        !Folder not on disk, then was deleted or renamed

        IF ~OMITTED(ClaQ_ClarionName) THEN       
            ClaQ_ClarionName  = SvSubQ:ClarionKey           !E.g. Clarion10 Clarion9.1
        END 
        IF ~OMITTED(ClaQ_VersioNo) THEN       
            ClaQ_VersioNo  = DEFORMAT(SvSubQ:ClarionKey)    !E.g. 10 or 9.1
        END
        ClaQ_RootPath = Root                 !E.g. C:\Clarion10  or C:\Cw10
        ADD(Cla_Queue)
    END
    RETURN

!####################################################################################

ClarionInstallsFromRegistryExplorer PROCEDURE() 

Clarion1Q QUEUE,PRE(Cla1Q) 
VerNo   STRING(5)         ! Cla1Q:VerNo
KeyName STRING(128)       ! Cla1Q:KeyName
Root    STRING(128)       ! Cla1Q:Root
          END

SV_SubKeysQ  QUEUE,PRE(SVSubQ)      !Keys under 'SOFTWARE\SoftVelocity'
SubKey          STRING(32)          !Should be Clarion# 
             END
             
Clarion2Q QUEUE,PRE(Cla2Q) 
SubKey  STRING(32)        ! Cla2Q:SubKey
Root    STRING(128)       ! Cla2Q:Root
KeyName STRING(128)       ! Cla2Q:KeyName
Problem STRING(128)       ! Cla2Q:Problem
          END
          
Window WINDOW('Clarion IDE Root Install Find'),AT(,,455,224),GRAY,SYSTEM,ICON(ICON:Clarion),FONT('Segoe UI',10,,FONT:regular)
        STRING('New Method:'),AT(5,2),USE(?Tab:NewWay),FONT(,,,FONT:regular+FONT:underline)
        STRING('Get All Sub Keys of SoftVelocity, then check each Sub Key for a ROOT value. This finds all new verisons.'), |
                AT(62,2),USE(?Tab:NewWay2)
        STRING('1. GetRegSubKeys(REG_LOCAL_MACHINE, ''SOFTWARE\SoftVelocity'', SV_SubKeys_Queue) '),AT(62,16),USE(?NewCode1), |
                FONT('Consolas')
        STRING('2. LOOP 1 TO RCORDS(SV_SubKeys_Queue) ... GET(SV_SubKeys_Queue,X) '),AT(62,24),USE(?NewCode2),FONT('Consolas')
        STRING('3. ROOT = GETREG(REG_LOCAL_MACHINE,''SOFTWARE\SoftVelocity\'' & SvSubQ:ClarionKey,''Root'')  '),AT(62,32), |
                USE(?NewCode3),FONT('Consolas')
        STRING('HKLM'),AT(6,23),USE(?SubKey1)
        STRING('\Software'),AT(6,33),USE(?SubKey2)
        STRING('\SoftVelocity'),AT(6,43),USE(?SubKey3)
        STRING('For each SubKey GETREG(HKLM, Software\SoftVelocity\ SubKey, Root)'),AT(78,43),USE(?New2)
        LIST,AT(6,54,64,55),USE(?LIST:SV_SubKeysQ),VSCROLL,FROM(SV_SubKeysQ),FORMAT('160L(2)|M~GetRegSubKeys~@s128@')
        LIST,AT(78,54,366,55),USE(?LIST:Clarion2Q),VSCROLL,FROM(Clarion2Q),FORMAT('48L(2)|M~SubKey~@s32@80L(2)|M~Root Fo' & |
                'lder~@s128@160L(2)|M~HKLM Registry Path~@s128@194L(2)|M~Reject ?~@s128@')
        PANEL,AT(55,122,387,2),USE(?PANEL1),BEVEL(0,0,0600H)
        STRING('Old Method:'),AT(3,117),USE(?Tab:OldWay),FONT(,,,FONT:regular+FONT:underline)
        STRING('This relied on finding known version numbers 8, 9, 9.1, 10, 11. This is broken by new versions e.g. 11.5' & |
                ' or 12.'),AT(15,129),USE(?Old1)
        STRING('Below shows same results as above using GETREG(HKLM, Software\SoftVelocity\Clarion'' & CHOOSE(V,8,9,9.1,' & |
                '10,11), ''Root'')'),AT(15,139),USE(?Old2)
        LIST,AT(15,151,327,50),USE(?LIST:Clarion1Q),VSCROLL,FROM(Clarion1Q),FORMAT('20C|M~Ver~@s5@160L(2)|M~HKLM Registr' & |
                'y Path~@s128@20L(2)|M~Root Folder~@s128@')
        STRING('To check for specific versions this method is fine, but it has no future.'),AT(15,206),USE(?Old3)
    END
    
    CODE
    OPEN(Window)
    DO GetReg_NewWay_Rtn
    DO GetReg_OldWay_Rtn
    ACCEPT
    END 
    
GetReg_NewWay_Rtn ROUTINE 
    DATA
X   LONG
!If_This_Was_for_Real    EQUATE( False )  !In Real code I would only look for 'Clarion' SubKeys
    CODE
    !Better way: Get Sub Keys of SoftVelocity, should return Clarion 91. Clarion10 Clarion11
    !            then for each SubKey do a GETREG of the ROOT Value
    GetRegSubKeys(REG_LOCAL_MACHINE, 'SOFTWARE\SoftVelocity', SV_SubKeysQ) 
    
    LOOP X=1 TO RECORDS(SV_SubKeysQ)
        GET(SV_SubKeysQ,X)
        CLEAR(Clarion2Q) 
        Cla2Q:SubKey  = SvSubQ:SubKey
        Cla2Q:KeyName = 'SOFTWARE\SoftVelocity\' & SvSubQ:SubKey  
        Cla2Q:Root    = GETREG(REG_LOCAL_MACHINE,Cla2Q:KeyName,'ROOT')   !Root Value
                
        IF lower(SvSubQ:SubKey[1:7])<>'clarion' THEN  !Should only take 'Clarion', SV could add other products.
           Cla2Q:Problem='<<> CLarion'                
        ELSIF ~Cla2Q:Root THEN                        !Blank indicates Root NOT found in Registry
           Cla2Q:Problem='Root Blank'
        ELSIF ~EXISTS(Cla2Q:Root) THEN                !If Folder not on disk then was deleted 
           Cla2Q:Problem='~Exists Root' 
        END !If Real
        
        ADD(Clarion2Q)
    END        
             
GetReg_OldWay_Rtn ROUTINE 
    DATA
F   LONG
CwVer STRING(4)
    CODE
    FREE(Clarion1Q) 
    LOOP F=1 TO 10
         CwVer=CHOOSE(F,'8','9','9.1','10','11','12','L','')
         IF CwVer='L' THEN
            CwVer=SUB(SYSTEM{PROP:LibVersion,2},1,2)  !E.g. C11=11000 work when 13 or 14 builds this
            IF CwVer <= 12 THEN BREAK.
         END
         IF ~CwVer THEN BREAK.
         CLEAR(Clarion1Q)
         Cla1Q:VerNo   = CwVer
         Cla1Q:KeyName = 'SOFTWARE\SoftVelocity\Clarion'& CwVer
         Cla1Q:Root=GETREG(REG_LOCAL_MACHINE,Cla1Q:KeyName,'Root')  
         IF ~Cla1Q:Root THEN CYCLE.
       !  IF ~EXISTS(ClaQ:Root) THEN CYCLE.
         ADD(Clarion1Q)
    END    