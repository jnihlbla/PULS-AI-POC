001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4042100.                                                
001500 AUTHOR.         BO SVENSSON.                                             
001600 DATE-WRITTEN.   96/12/02.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS IS A QUESTION PROGRAM ON CUSTOMER DATABASE.                 
002100*        GIVEN DISTRICT AND CUSTOMERNO WILL DISPLAY INFO                  
002200*        ABOUT THIS GOODS RECIVER.                                        
002300*        IF ALSO DC IS GIVEN INFO ABOUT DELIVERIES FROM THIS DC           
002400*        TO THE GOODS RECIEVER IS ADDED.                                  
002500*                                                                         
002601*        THE PROGRAM READS     WLGMTA (WDB2)                              
002602*        THE PROGRAM READS     WLGMTB (WDB3)                              
002604*        THE PROGRAM READS     WLBETC (WDB1)                              
002605*        THE PROGRAM READS     WDB6                                       
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSACTION: W4T421                                              
003000*        MID:         W4I42101                                            
003100*                                                                         
003200*    OUTDATA.                                                             
003300*        MOD:         W4O42101                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
003901*    -COPY WY2000W1                                                       
003902     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W4042100'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400                                                                          
004410 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
004420 77  MAX-INDX                    PIC S9(4)   VALUE +7  COMP SYNC.         
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004601                                                                          
004620 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004700                                                                          
004900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005100                                                                          
005300                                                                          
005400 77  SW-GMT-EXISTS               PIC X       VALUE 'N'.                   
005500     88  GMT-EXISTS                          VALUE 'Y'.                   
005600     88  GMT-NOT-EXISTS                      VALUE 'N'.                   
005601                                                                          
005610 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005620     88  KEYS-OK                             VALUE 'Y'.                   
005630     88  KEYS-WRONG                          VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  OWN-MID                             VALUE '4421'.                
006000     88  GOOD-MID                            VALUE '4421' '4422'          
006100                                                   '4423' '4424'          
006200                                                   '4425' '4426'          
006300                                                   '4427' '4428'          
006400                                                   '4429'.                
006500     88  HELP-MID                            VALUE '0551'.                
006501                                                                          
006591     EJECT                                                                
006593 01  W-ROPACK-SOK                PIC X(1)    VALUE SPACE.                 
006594 01  W-RO-TEXT                   PIC X(25)   VALUE SPACE.                 
006595                                                                          
006596 01  FILLER                      PIC X(16)   VALUE 'ROPACKTAB'.           
006597     SKIP3                                                                
006598 01  ROPACK-RADER.                                                        
006599     03  FILLER  PIC X(27) VALUE 'A ALL CLASS 3 AND 4        '.           
006600     03  FILLER  PIC X(27) VALUE 'B ALL CLASS 2, 3 AND 4     '.           
006601     03  FILLER  PIC X(27) VALUE 'C ALL CLASS 1              '.           
006602     03  FILLER  PIC X(27) VALUE 'D ALL CLASS 1 AND 2        '.           
006603     03  FILLER  PIC X(27) VALUE 'E ALL TPO                  '.           
006604     03  FILLER  PIC X(27) VALUE 'F SAME ACC + COST-C G/N INV'.           
006605     03  FILLER  PIC X(27) VALUE 'G SAME ORDER CLASS AND FC  '.           
006606     03  FILLER  PIC X(27) VALUE 'H ONLY BO                  '.           
006607     03  FILLER  PIC X(27) VALUE 'I ALL BO WITH SAME FC      '.           
006608     03  FILLER  PIC X(27) VALUE 'J ALL BO CLASS 3 AND 4     '.           
006609     03  FILLER  PIC X(27) VALUE 'K ALL BO CLASS 2, 3 AND 4  '.           
006610     03  FILLER  PIC X(27) VALUE 'L OUTPOINT. BO/DO FROM PROF'.           
006611     03  FILLER  PIC X(27) VALUE '0 NO SUBPACKING            '.           
006612     03  FILLER  PIC X(27) VALUE '1 ALL                      '.           
006613     03  FILLER  PIC X(27) VALUE '2 SAME ORDER CLASS         '.           
006614     03  FILLER  PIC X(27) VALUE '3 SAME ORDER CLASS OR LOWER'.           
006615     03  FILLER  PIC X(27) VALUE '4 SAME FC                  '.           
006616     03  FILLER  PIC X(27) VALUE '5 OUTPOINTED BO/DO         '.           
006617     03  FILLER  PIC X(27) VALUE '6 ODD/EVEN WITH SAME FC    '.           
006620     03  FILLER  PIC X(27) VALUE '8 ALL EXEPT OUTPOINTED     '.           
006630     03  FILLER  PIC X(27) VALUE '9 SAME FC AS ACTUAL ORDER  '.           
006688 01  FILLER REDEFINES ROPACK-RADER.                                       
006689     03  ROPACK-TABELL       OCCURS 21                                    
006690                              ASCENDING KEY IS ROPACK-KDROPACK            
006691                              INDEXED BY ROPACK-IX.                       
006692         05  ROPACK-KDROPACK     PIC X(1).                                
006693         05  FILLER              PIC X(1).                                
006694         05  ROPACK-TEXT         PIC X(25).                               
006695                                                                          
006696     EJECT                                                                
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007010     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007600*01 -COPY WMEDAREA                                                        
007610     EJECT                                                                
007620 01  FILLER                      PIC X(16)   VALUE 'WSEC-AREA  '.         
007630*01  -COPY WSECAREA                                                       
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
008100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008200     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
008300     EJECT                                                                
008400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700     SKIP3                                                                
008800*01 -COPY WMSGINIT                                                        
009000     EJECT                                                                
009100*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009400     SKIP3                                                                
009500*01  MID -COPY W4I42101                                                   
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009800     SKIP3                                                                
009900*01  -COPY WMSGAREA                                                       
010000     EJECT                                                                
010100     03  MOD REDEFINES MSG-AREA.                                          
010200*      05  -COPY W4O42101                                                 
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010500     SKIP3                                                                
010600*01  -COPY WMFSAREA                                                       
010700     EJECT                                                                
010800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010900*                                                                         
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011200     SKIP3                                                                
011300 01  KEYS-TO-DLI.                                                         
011401     03  W-IDGMT-X.                                                       
011402         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
011403         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
011404     03  W-WDB301KY-X.                                                    
011405         05  W-IDDC-301          PIC X(2)         VALUE SPACE.            
011406         05  W-IDDISTR-301       PIC S9(5) COMP-3 VALUE ZERO.             
011407         05  W-IDKUNDNR-301      PIC S9(7) COMP-3 VALUE ZERO.             
011408     03  W-WDB301KY-DEF.                                                  
011409         05  W-IDDC-DEF          PIC X(2)         VALUE SPACE.            
011410         05  W-IDDISTR-DEF       PIC S9(5) COMP-3 VALUE ZERO.             
011411         05  W-IDKUNDNR-DEF      PIC S9(7) COMP-3 VALUE +9999999.         
011412     03  W-WDB1-IDPARTNR-X.                                               
011413         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
011414                                                                          
011415     03  W-IDDC-B6-X.                                                     
011416         05 W-IDDC-B6                  PIC X(2).                          
011417                                                                          
011500     SKIP2                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FOUND                       VALUE '  '.                  
011900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012100     SKIP2                                                                
012200 01  GOOD-STATUSCODES.                                                    
012300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     SKIP3                                                                
012500 01  SSA1                        PIC X(64).                               
012600 01  SSA2                        PIC X(64).                               
012700     EJECT                                                                
012800*    --- IMS FUNCTION CODES                                               
012900*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300                                                                          
013401 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTA01'.                    
013402 01  DLI-IO-WLGMTA01.                                                     
013403*    03  -COPY WDB201                                                     
013404 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTB01'.                    
013405 01  DLI-IO-WLGMTB01.                                                     
013406*    03  -COPY WDB301                                                     
013410 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBETC01'.                    
013411 01  DLI-IO-WLBETC01.                                                     
013412*    03  -COPY WDB101                                                     
013413                                                                          
013414 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013415 01   DLI-IO-AREA-B601.                                                   
013416*     03  -COPY WDB601                                                    
013417                                                                          
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900*01  -COPY W0009   -PRE MSG-                                              
014000*01  -COPY W0008   -PRE USEA-                                             
014100     05  FILLER                  PIC X.                                   
014201     EJECT                                                                
014202*01  -COPY W0008  -PRE GMTA-                                              
014203     05  FILLER                  PIC X.                                   
014204     EJECT                                                                
014205*01  -COPY W0008  -PRE GMTB-                                              
014206     05  FILLER                  PIC X.                                   
014210     EJECT                                                                
014211*01  -COPY W0008  -PRE BETC-                                              
014212     05  FILLER                  PIC X.                                   
014213     EJECT                                                                
014214*01  -COPY W0008  -PRE WDB6-                                              
014215     05  FILLER                  PIC X.                                   
014216     EJECT                                                                
014401 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
014402     BETC-PCB WDB6-PCB.                                                   
014403 MAIN SECTION.                                                            
014404     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
014410     BETC-PCB WDB6-PCB.                                                   
014500                                                                          
014700     PERFORM IMS-GET-MSG                                                  
014800     IF SEGMENT-FOUND                                                     
014900       PERFORM A-INIT                                                     
015000       PERFORM B-CHECK-KEYS                                               
015100       IF KEYS-OK                                                         
015600         PERFORM F-READ-SHOW-INFO                                         
015700       END-IF                                                             
015800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015900*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O42101 + 4                      
016100       PERFORM IMS-INSERT-MSG                                             
016200     END-IF                                                               
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000                                                                          
017100     IF MSG-DOUBLE-TRANSACTIONS                                           
017200       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I42101                 
017300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017500     ELSE                                                                 
017600       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I42101                  
017700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017900     END-IF                                                               
018000                                                                          
018100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018400                                                                          
018500     MOVE LOW-VALUE TO MSG-AREA                                           
018600     MOVE 'W4O421N1' TO MFS-IDMOD                                         
018700     MOVE '4421' TO MOD-IDTRANS                                           
018800     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
018900                                                                          
019000     IF OWN-MID OR HELP-MID                                               
019100       CONTINUE                                                           
019200     ELSE                                                                 
019300       MOVE SPACE TO MFS-KDTRTYP                                          
019400       MOVE '7' TO MFS-IDPFK                                              
019500     END-IF                                                               
019600                                                                          
019700     ACCEPT TODAYS-DATE  FROM DATE                                        
019800     .                                                                    
019900     EJECT                                                                
020000 B-CHECK-KEYS SECTION.                                                    
020100                                                                          
020200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020300     MOVE '001'             TO MSGI-KDCALL                                
020400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020600     MOVE '4421'            TO MSGI-IDTRANS                               
020700     IF GOOD-MID                                                          
020801         MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                            
020802         MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                           
020900     END-IF                                                               
021000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021010     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
021100                                                                          
021200     MOVE YES TO KEYS-SW                                                  
021300                                                                          
021401                                                                          
021402*    -- CHECK OF IDDISTR                                                  
021403     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
021404                                                                          
021405     IF MID-IDDISTR-IN NOT = ALL '+'                                      
021406       MOVE '7'         TO MFS-IDPFK                                      
021407       MOVE SPACE       TO MFS-KDTRTYP                                    
021408     END-IF                                                               
021409     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
021410     IF MSGI-IDDISTR NUMERIC                                              
021411       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
021412                            W-IDDISTR-301                                 
021413                            W-IDDISTR-DEF                                 
021414     ELSE                                                                 
021415       MOVE NOO TO KEYS-SW                                                
021416     END-IF                                                               
021418                                                                          
021419*    -- CHECK OF IDKUNDNR                                                 
021420     MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-IN                              
021421                                                                          
021422     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
021423       MOVE '7'         TO MFS-IDPFK                                      
021424       MOVE SPACE       TO MFS-KDTRTYP                                    
021425     END-IF                                                               
021426     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
021427     IF MSGI-IDKUNDNR NUMERIC                                             
021428       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
021429                             W-IDKUNDNR-301                               
021430       MOVE +9999999      TO W-IDKUNDNR-DEF                               
021431     ELSE                                                                 
021432       MOVE NOO TO KEYS-SW                                                
021433     END-IF                                                               
021435                                                                          
021436*    -- CHECK OF IDDC                                                     
021437     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
021438                                                                          
021439     IF MID-IDDC-IN NOT = ALL '+'                                         
021440       MOVE '7'         TO MFS-IDPFK                                      
021441       MOVE SPACE       TO MFS-KDTRTYP                                    
021442       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
021443     ELSE                                                                 
021444       MOVE MID-IDDC-UT TO W-IDDC-B6                                      
021445     END-IF                                                               
021446     PERFORM IMS-GU-WDB601                                                
021447                                                                          
021448     IF  DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                        
021453         MOVE MSGI-IDDC TO W-IDDC-B6                                      
021454         PERFORM IMS-GU-WDB601                                            
021455     END-IF                                                               
021460                                                                          
021502     IF GOOD-MID OR KEYS-OK                                               
021503       MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                           
021504       INSPECT MOD-IDDISTR-UT    REPLACING LEADING ZERO BY SPACE          
021507       MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                          
021508       INSPECT MOD-IDKUNDNR-UT   REPLACING LEADING ZERO BY SPACE          
021509       MOVE DCS-IDDC          TO MOD-IDDC-UT                              
021510     ELSE                                                                 
021511       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                             
021512       MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-UT                            
021513       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
021520     END-IF                                                               
021600                                                                          
021700     IF KEYS-WRONG                                                        
021800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021900       CALL WMEDKONV USING MED-WMEDAREA                                   
022000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022200       PERFORM MFS-ERASE-FIELD-OUT                                        
022210     ELSE                                                                 
022220                                                                          
022230       MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                            
022240       MOVE '4421'            TO    SEC-IDTRANS                           
022250       MOVE MSGI-IDDISTR      TO    SEC-IDKEY                             
022260                                                                          
022270       CALL WSECURIT          USING SEC-IDUSER                            
022280                                    SEC-IDTRANS                           
022290                                    SEC-IDKEY                             
022291                                    SEC-KDSVAR                            
022292                                                                          
022293       IF SEC-KDSVAR = 'F'                                                
022294         MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                
022296         CALL WMEDKONV USING MED-WMEDAREA                                 
022297         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022298         PERFORM MFS-ERASE-FIELD-OUT                                      
022299         MOVE NOO          TO KEYS-SW                                     
022300       ELSE                                                               
022301          CONTINUE                                                        
022302       END-IF                                                             
022310     END-IF                                                               
022400     .                                                                    
022600     EJECT                                                                
022800 F-READ-SHOW-INFO SECTION.                                                
022900                                                                          
023000     PERFORM FA-READ-BASICDATA                                            
023100                                                                          
023120     IF GMT-NOT-EXISTS                                                    
023200*    IF SEGMENT-MISSING                                                   
023300* MOVE RIGHT ERRORMESSAGE                                                 
023400*       CALL WMEDKONV USING MED-WMEDAREA                                  
023500*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023510        MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL                     
023600        PERFORM MFS-ERASE-FIELD-OUT                                       
023700     ELSE                                                                 
023710                                                                          
023720        MOVE GMT-IDPARTNR      TO W-WDB1-IDPARTNR                         
023730        PERFORM IMS-GET-BETC                                              
023731        IF  SEGMENT-FOUND                                                 
023740            IF  BET-KDKREDSP = 0                                          
023750                MOVE NOO           TO MOD-KDKREDSP                        
023760            ELSE                                                          
023770                MOVE YES           TO MOD-KDKREDSP                        
023780            END-IF                                                        
023785        ELSE                                                              
023786            MOVE MFS-ERASE-FIELD   TO MOD-KDKREDSP                        
023795        END-IF                                                            
023796                                                                          
023800        MOVE DCS-IDDC   TO W-IDDC-301                                     
023810                           W-IDDC-DEF                                     
023820        PERFORM IMS-GET-GMTB                                              
023830        IF  SEGMENT-FOUND                                                 
023840            MOVE DC-IDGMTOMR TO MOD-IDGMTOMR                              
023850            MOVE DC-IDPRCTAB TO MOD-IDPRCTAB                              
023860            MOVE DC-IDPKLTAB TO MOD-IDPKLTAB                              
023870            MOVE DC-KDROPACK-DAG  TO MOD-KDROPACK-DAG                     
023880            MOVE DC-KDROPACK-BULK TO MOD-KDROPACK-BULK                    
023881                                                                          
023884            MOVE DC-KDROPACK-DAG  TO W-ROPACK-SOK                         
023885            PERFORM FB-ROPACK-TEXT                                        
023886            MOVE W-RO-TEXT        TO MOD-BERODAG-X25                      
023887            MOVE DC-KDROPACK-BULK TO W-ROPACK-SOK                         
023888            PERFORM FB-ROPACK-TEXT                                        
023889            MOVE W-RO-TEXT        TO MOD-BEROBULK-X25                     
023890                                                                          
023894            MOVE DC-KVLEDTIM-0    TO MOD-KVLEDTIM-0                       
023895            MOVE DC-KVLEDTIM-1    TO MOD-KVLEDTIM-1                       
023896            MOVE DC-KVLEDTIM-2    TO MOD-KVLEDTIM-2                       
023897            MOVE DC-KVLEDTIM-3    TO MOD-KVLEDTIM-3                       
023898            MOVE DC-KVLEDTIM-4    TO MOD-KVLEDTIM-4                       
023899                                                                          
023925        ELSE                                                              
023926            MOVE 'NO SUPPORT FROM GIVEN DC' TO MOD-TEMFSFEL               
023927            PERFORM MFS-ERASE-FIELD-OUT-DC                                
023928        END-IF                                                            
023930     END-IF                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 FA-READ-BASICDATA SECTION.                                               
024300                                                                          
024400     PERFORM IMS-GET-GMTA                                                 
024500                                                                          
024600     IF  SEGMENT-FOUND                                                    
024601         MOVE YES               TO SW-GMT-EXISTS                          
024610         MOVE GMT-BEGMT-RAD1    TO MOD-BEGMT-RAD1                         
024620         MOVE GMT-BEGMT-RAD2    TO MOD-BEGMT-RAD2                         
024630         MOVE GMT-ADGMT-GATA    TO MOD-ADGMT-GATA                         
024640         MOVE GMT-ADGMT-PADR    TO MOD-ADGMT-PADR                         
024650         MOVE GMT-ADGMT-LAND    TO MOD-ADGMT-LAND                         
024660         MOVE GMT-IDPARTNR      TO MOD-IDPARTNR                           
024693         MOVE GMT-IDLEVNR       TO MOD-IDLEVNR                            
024694         IF  GMT-FLCOD = 'J'                                              
024695             MOVE YES           TO MOD-FLCOD                              
024696         ELSE                                                             
024697             MOVE GMT-FLCOD     TO MOD-FLCOD                              
024698         END-IF                                                           
024700         IF  GMT-FLSAMFAK = 'J'                                           
024701             MOVE YES           TO MOD-FLSAMFAK                           
024702         ELSE                                                             
024703             MOVE GMT-FLSAMFAK  TO MOD-FLSAMFAK                           
024704         END-IF                                                           
024705         MOVE GMT-IDKUNDNR-HEAD TO MOD-IDKUNDNR-HEAD                      
024706         IF  GMT-FLOKFAK-G = 'J'                                          
024707             MOVE YES           TO MOD-FLOKFAK-G                          
024708         ELSE                                                             
024709             MOVE GMT-FLOKFAK-G TO MOD-FLOKFAK-G                          
024710         END-IF                                                           
024711         IF  GMT-FLOKFAK-K = 'J'                                          
024712             MOVE YES           TO MOD-FLOKFAK-K                          
024713         ELSE                                                             
024714             MOVE GMT-FLOKFAK-K TO MOD-FLOKFAK-K                          
024715         END-IF                                                           
024716         IF  GMT-FLOKFAK-N = 'J'                                          
024717             MOVE YES           TO MOD-FLOKFAK-N                          
024718         ELSE                                                             
024719             MOVE GMT-FLOKFAK-N TO MOD-FLOKFAK-N                          
024720         END-IF                                                           
024721         IF  GMT-FLOKFAK-R = 'J'                                          
024722             MOVE YES           TO MOD-FLOKFAK-R                          
024723         ELSE                                                             
024724             MOVE GMT-FLOKFAK-R TO MOD-FLOKFAK-R                          
024725         END-IF                                                           
024727         MOVE GMT-KDGENFAK      TO MOD-KDGENFAK                           
024728                                                                          
024729         MOVE +1 TO INDX                                                  
024730         PERFORM UNTIL INDX > MAX-INDX                                    
024731           MOVE GMT-IDDC-BULK(INDX) TO MOD-IDDC-BULK(INDX)                
024732           MOVE GMT-IDDC-DAY(INDX)  TO MOD-IDDC-DAY(INDX)                 
024733           MOVE GMT-IDDC-VOR(INDX)  TO MOD-IDDC-VOR(INDX)                 
024740           ADD +1 TO INDX                                                 
024750         END-PERFORM                                                      
024760                                                                          
024770         MOVE +1 TO INDX                                                  
024780         PERFORM UNTIL INDX > MAX-INDX                                    
024790           IF  GMT-IDDC-BULK(INDX) NOT = SPACE                            
024791               MOVE GMT-IDDC-BULK(INDX) TO W-IDDC-301                     
024792                                           W-IDDC-DEF                     
024793               PERFORM IMS-GET-GMTB-2                                     
024794               IF  SEGMENT-MISSING                                        
024795                   MOVE MFS-ERASE-FIELD                                   
024796                                      TO MOD-KDGENFRA-MO(INDX)            
024797                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
024798                                      TO MOD-TEMFSFEL                     
024799               ELSE                                                       
024800                   MOVE DC-KDGENFRA-MO                                    
024801                                      TO MOD-KDGENFRA-MO(INDX)            
024802               END-IF                                                     
024803           ELSE                                                           
024804               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-MO(INDX)            
024805           END-IF                                                         
024806                                                                          
024807           IF  GMT-IDDC-DAY(INDX) NOT = SPACE                             
024808               MOVE GMT-IDDC-DAY(INDX) TO W-IDDC-301                      
024809                                          W-IDDC-DEF                      
024810               PERFORM IMS-GET-GMTB-2                                     
024811               IF  SEGMENT-MISSING                                        
024812                   MOVE MFS-ERASE-FIELD                                   
024813                                      TO MOD-KDGENFRA-DO(INDX)            
024815                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
024816                                      TO MOD-TEMFSFEL                     
024817               ELSE                                                       
024818                   MOVE DC-KDGENFRA-DO                                    
024819                                      TO MOD-KDGENFRA-DO(INDX)            
024820               END-IF                                                     
024821           ELSE                                                           
024822               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-DO(INDX)            
024823           END-IF                                                         
024824                                                                          
024825           IF  GMT-IDDC-VOR(INDX) NOT = SPACE                             
024826               MOVE GMT-IDDC-VOR(INDX) TO W-IDDC-301                      
024827                                          W-IDDC-DEF                      
024828               PERFORM IMS-GET-GMTB-2                                     
024829               IF  SEGMENT-MISSING                                        
024830                   MOVE MFS-ERASE-FIELD                                   
024831                                      TO MOD-KDGENFRA-VOR(INDX)           
024833                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
024834                                      TO MOD-TEMFSFEL                     
024835               ELSE                                                       
024836                   MOVE DC-KDGENFRA-VOR                                   
024837                                      TO MOD-KDGENFRA-VOR(INDX)           
024838               END-IF                                                     
024839           ELSE                                                           
024840               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-VOR(INDX)           
024841           END-IF                                                         
024842                                                                          
024843           ADD +1 TO INDX                                                 
024844         END-PERFORM                                                      
024845                                                                          
024846         MOVE GMT-TISTADAT   TO TMP1-YYMMDD                               
024847         MOVE TODAYS-DATE    TO TMP2-YYMMDD                               
024848         PERFORM WY2000P1                                                 
024849         IF  TMP1-YYMMDD > TMP2-YYMMDD                                    
024850         OR  GMT-TISTADAT  = ZERO                                         
024851             MOVE 'GOODS RECEIVER NOT READY FOR ORDER'                    
024852                                      TO MOD-TEMFSINF                     
024853         END-IF                                                           
024854                                                                          
024855         MOVE GMT-TISTODAT   TO TMP1-YYMMDD                               
024856         MOVE TODAYS-DATE    TO TMP2-YYMMDD                               
024857         PERFORM WY2000P1                                                 
024858         IF  TMP1-YYMMDD <= TMP2-YYMMDD                                   
024859         AND GMT-TISTODAT  > ZERO                                         
024860             MOVE 'GOODS RECEIVER NOT READY FOR ORDER'                    
024861                                      TO MOD-TEMFSINF                     
024862         END-IF                                                           
024863                                                                          
024870     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025010 FB-ROPACK-TEXT SECTION.                                                  
025020                                                                          
025030       SEARCH ALL ROPACK-TABELL                                           
025040          AT END                                                          
025050             MOVE SPACE TO W-RO-TEXT                                      
025070          WHEN                                                            
025080             ROPACK-KDROPACK (ROPACK-IX) = W-ROPACK-SOK                   
025090             MOVE ROPACK-TEXT (ROPACK-IX) TO W-RO-TEXT                    
025092       END-SEARCH                                                         
025093     .                                                                    
025100     EJECT                                                                
025200 MFS-ERASE-FIELD-OUT SECTION.                                             
025300                                                                          
025400*    --- ALLA UTDATA-FÄLT                                                 
025600     MOVE MFS-ERASE-FIELD TO MOD-BEGMT-RAD1                               
025700                             MOD-BEGMT-RAD2                               
025710                             MOD-ADGMT-GATA                               
025720                             MOD-ADGMT-PADR                               
025730                             MOD-ADGMT-LAND                               
025740                             MOD-IDPARTNR                                 
025770                             MOD-IDLEVNR                                  
025791                             MOD-KDKREDSP                                 
025792                             MOD-FLCOD                                    
025793                             MOD-FLSAMFAK                                 
025794                             MOD-IDKUNDNR-HEAD                            
025795                             MOD-KDROPACK-DAG                             
025796                             MOD-BERODAG-X25                              
025797                             MOD-KDROPACK-BULK                            
025798                             MOD-BEROBULK-X25                             
025799                             MOD-IDGMTOMR                                 
025800                             MOD-IDPRCTAB                                 
025801                             MOD-IDPKLTAB                                 
025810                             MOD-FLOKFAK-G                                
025820                             MOD-FLOKFAK-K                                
025830                             MOD-FLOKFAK-N                                
025840                             MOD-FLOKFAK-R                                
025850                             MOD-KDGENFAK                                 
025860                             MOD-KVLEDTIM-0                               
025870                             MOD-KVLEDTIM-1                               
025880                             MOD-KVLEDTIM-2                               
025890                             MOD-KVLEDTIM-3                               
025891                             MOD-KVLEDTIM-4                               
025892     MOVE +1 TO INDX                                                      
025893     PERFORM UNTIL INDX > MAX-INDX                                        
025894       PERFORM MFS-ERASE-LINE-FIELD                                       
025895       ADD +1 TO INDX                                                     
025896     END-PERFORM                                                          
025900     .                                                                    
025910     SKIP3                                                                
025920 MFS-ERASE-LINE-FIELD SECTION.                                            
025930                                                                          
025940     MOVE MFS-ERASE-FIELD TO MOD-IDDC-BULK    (INDX)                      
025950                             MOD-KDGENFRA-MO  (INDX)                      
025960                             MOD-IDDC-DAY     (INDX)                      
025970                             MOD-KDGENFRA-DO  (INDX)                      
025980                             MOD-IDDC-VOR     (INDX)                      
025990                             MOD-KDGENFRA-VOR (INDX)                      
025991     .                                                                    
025992     EJECT                                                                
025993 MFS-ERASE-FIELD-OUT-DC SECTION.                                          
025994                                                                          
025995*    --- DC FIELDS                                                        
025996     MOVE MFS-ERASE-FIELD TO MOD-KDROPACK-DAG                             
027100                             MOD-BERODAG-X25                              
027200                             MOD-KDROPACK-BULK                            
027300                             MOD-BEROBULK-X25                             
027400                             MOD-IDGMTOMR                                 
027500                             MOD-IDPRCTAB                                 
027600                             MOD-IDPKLTAB                                 
028200                             MOD-KVLEDTIM-0                               
028300                             MOD-KVLEDTIM-1                               
028400                             MOD-KVLEDTIM-2                               
028500                             MOD-KVLEDTIM-3                               
028600                             MOD-KVLEDTIM-4                               
029200     .                                                                    
029700     EJECT                                                                
029800* --- IMS SECTIONS ---                                                    
029900     SKIP3                                                                
030000 IMS-GET-MSG SECTION.                                                     
030100                                                                          
030200     MOVE '  QC' TO GOOD-STATUSCODES                                      
030300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030500     PERFORM IMS-STATUSCHECK                                              
030600     .                                                                    
030700     SKIP3                                                                
030800 IMS-INSERT-MSG SECTION.                                                  
030900                                                                          
031000     IF MSGI-IDLAND-SPR = 'GB'                                            
031100       MOVE 'N' TO MFS-KDHUVOMR                                           
031200     END-IF                                                               
031300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031400     MOVE SPACE TO GOOD-STATUSCODES                                       
031500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031700     PERFORM IMS-STATUSCHECK                                              
031800     .                                                                    
031901     EJECT                                                                
031902 IMS-GET-GMTA SECTION.                                                    
031903                                                                          
031904     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
031905          DELIMITED BY SIZE INTO SSA1                                     
031906     MOVE '  GE' TO GOOD-STATUSCODES                                      
031907     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
031908     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
031909     PERFORM IMS-STATUSCHECK                                              
031910     .                                                                    
031911     EJECT                                                                
031912 IMS-GET-GMTB SECTION.                                                    
031913                                                                          
031915     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
031916                    '+WDB301KY =' W-WDB301KY-DEF ')'                      
031917          DELIMITED BY SIZE INTO SSA1                                     
031918     MOVE '  GE' TO GOOD-STATUSCODES                                      
031919     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-WLGMTB01 SSA1                  
031920     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
031921     PERFORM IMS-STATUSCHECK                                              
031930     .                                                                    
031931     EJECT                                                                
031932 IMS-GET-GMTB-2 SECTION.                                                  
031933                                                                          
031934     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
031935                    '+WDB301KY =' W-WDB301KY-DEF ')'                      
031936          DELIMITED BY SIZE INTO SSA1                                     
031937     MOVE '  GE' TO GOOD-STATUSCODES                                      
031938     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-WLGMTB01 SSA1                  
031939     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
031940     PERFORM IMS-STATUSCHECK                                              
031941     .                                                                    
031942     EJECT                                                                
031943 IMS-GET-BETC SECTION.                                                    
031944                                                                          
031945     STRING 'WLBETC01(IDPARTNR =' W-WDB1-IDPARTNR-X ')'                   
031946          DELIMITED BY SIZE INTO SSA1                                     
031947     MOVE '  GE' TO GOOD-STATUSCODES                                      
031948     CALL CBLTDLI USING GU BETC-PCB DLI-IO-WLBETC01 SSA1                  
031949     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
031950     PERFORM IMS-STATUSCHECK                                              
031951     .                                                                    
032000     EJECT                                                                
032010 IMS-GU-WDB601    SECTION.                                                
032020     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032030          DELIMITED BY SIZE INTO SSA1                                     
032040     MOVE '  GE' TO GOOD-STATUSCODES                                      
032050     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
032060     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032070     PERFORM IMS-STATUSCHECK                                              
032080     IF SEGMENT-MISSING                                                   
032090         MOVE SPACE TO DCS-KDDC                                           
032091     END-IF                                                               
032092     .                                                                    
032100 IMS-STATUSCHECK SECTION.                                                 
032200                                                                          
032300     SET STATUS-IX TO 1                                                   
032400     SEARCH GOOD-STATUS                                                   
032500       AT END                                                             
032600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
032700         DELIMITED BY SIZE INTO ERROR-TEXT                                
032800         CALL FELLOG                                                      
032900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033000         CONTINUE                                                         
033100     END-SEARCH                                                           
033200     .                                                                    
033210     EJECT                                                                
033300*    -COPY WY2000P1                                                       
