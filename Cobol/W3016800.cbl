001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3016800.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   00/05/08.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PGM VISAR INVESTERINGSINFORMATION                                
002100*        FÖR BYTES-ARTIKLAR                                               
002200*                                                                         
002310*        PROGRAMMET LÄSER      WDA9                                       
002311*                              WDA9 VIA WDA9BSEQ                          
002320*                              WDD3                                       
002330*                              WDK6                                       
002340*                              WDL8                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W3T168                                              
002700*        MID:         W3I16801                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W3O16801                                            
003100                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701*    -COPY WY2000W6                                                       
003710     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W3016800'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005200 77  SOK-RECORDS-SW              PIC X       VALUE 'J'.                   
005300     88  RECORDS-MISSING                     VALUE 'J'.                   
005500                                                                          
005501 77  SOK-FKNGRP-SW               PIC X       VALUE SPACE.                 
005502     88  SOK-FKNGRP                          VALUE 'J'.                   
005503     88  SOK-ARTNR                           VALUE 'N'.                   
005504                                                                          
005510 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005520     88  NYCKLAR-OK                          VALUE 'J'.                   
005530     88  NYCKLAR-FEL                         VALUE 'N'.                   
005540                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '3168'.                
005800     88  GODK-MID                            VALUE '3167'                 
005900                                                   '3168'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006410                                                                          
006411*    --- DIVERSE                                                          
006412 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
006413 77  MAX-INDX                    PIC S9(4)   VALUE +53  COMP SYNC.        
006414 01  WS-IDARTNR-9                PIC  9(9).                               
006415 01  WS-IDARTNR-8                PIC  X(8).                               
006420 01  WS-TIAA-FOM                 PIC  9(2).                               
006421 01  WS-TIAA-TOM                 PIC  9(2).                               
006422 01  WS-TIAAAA-FOM               PIC  9(4)    VALUE 2000.                 
006423 01  WS-TIAAAA-TOM               PIC  9(4)    VALUE 2000.                 
006424 01  WS-DAAAPP-FOM               PIC  9(6)    VALUE 200000.               
006425 01  WS-TIAAPP-FOM               PIC  9(4).                               
006426 01  WS-TIAAPP-FOM-ALFA.                                                  
006427     03  FILLER                  PIC  X(2).                               
006428     03  WS-TIAAPP-FOM-2-4       PIC  X(2).                               
006429 01  WS-DAAAPP-TOM               PIC  9(6)    VALUE 200000.               
006430 01  WS-TIAAPP-TOM               PIC  9(4).                               
006431 01  WS-TIAAPP-TOM-ALFA.                                                  
006432     03  FILLER                  PIC  X(2).                               
006433     03  WS-TIAAPP-TOM-2-4       PIC  X(2).                               
006434 01  WS-RECOST                   PIC S9(9)V99 VALUE ZERO COMP-3.          
006435 01  WS-SUINVEST-DC              PIC S9(9)    VALUE ZERO COMP-3.          
006436 01  WS-SUINVEST-REM             PIC S9(9)    VALUE ZERO COMP-3.          
006437 01  WS-SUINVEST-TOT             PIC S9(9)    VALUE ZERO COMP-3.          
006438 01  WS-SUINVEST-CORE            PIC S9(9)    VALUE ZERO COMP-3.          
006439 01  WS-SULEVANT-DC              PIC S9(9)    VALUE ZERO COMP-3.          
006440 01  WS-REPROCENT                PIC S9(3)    VALUE ZERO COMP-3.          
006441                                                                          
006442 01  WS-IDFKNGRP-JFR             PIC S9(5)    VALUE ZERO COMP-3.          
006443 01  WS-IDARTNR-JFR              PIC S9(9)    VALUE ZERO COMP-3.          
006444     EJECT                                                                
006445                                                                          
006450 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
006460 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
006470*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
006480     EJECT                                                                
006490                                                                          
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006610     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007200     EJECT                                                                
007210                                                                          
007220 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007230 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007240 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007250     EJECT                                                                
007260                                                                          
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007410     EJECT                                                                
007500                                                                          
007510*01  -COPY WDATAREA                                                       
007520     EJECT                                                                
007530                                                                          
007600 01  MESSAGE-CODES.                                                       
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
008100     EJECT                                                                
008110                                                                          
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008810                                                                          
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300*01  MID -COPY W3I16801                                                   
009400     EJECT                                                                
009410                                                                          
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700*01  -COPY WMSGAREA                                                       
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W3O16801                                                 
010100     EJECT                                                                
010110                                                                          
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010510                                                                          
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000 01  NYCKLAR-TILL-DLI.                                                    
011101     03  W-IDARTNR-A9-X.                                                  
011102         05  W-IDARTNR-A9        PIC S9(9)   VALUE ZERO COMP-3.           
011103     03  W-IDDISTR-A9-X.                                                  
011104         05  W-IDDISTR-A9        PIC S9(5)   VALUE ZERO COMP-3.           
011105     03  W-DAAAPP-A9-MIN-X.                                               
011110         05  W-DAAAPP-A9-MIN     PIC  9(6)   VALUE ZERO.                  
011111     03  W-DAAAPP-A9-MAX-X.                                               
011112         05  W-DAAAPP-A9-MAX     PIC  9(6)   VALUE ZERO.                  
011135     03  W-WDA9BSEQ-X.                                                    
011136         05  W-IDFKNGRP-A9B      PIC S9(5)   VALUE ZERO COMP-3.           
011137     03  W-IDARTNR-A9B-X.                                                 
011138         05  W-IDARTNR-A9B       PIC S9(8)   VALUE ZERO COMP-3.           
011140     03  W-IDARTNR-D3-X.                                                  
011150         05  W-IDARTNR-D3        PIC S9(9)   VALUE ZERO COMP-3.           
011160     03  W-IDSKYLT-D3-X.                                                  
011170         05  W-IDSKYLT-D3        PIC X(3)    VALUE 'GB'.                  
011180     03  W-IDARTNR-K6-X.                                                  
011190         05  W-IDARTNR-K6        PIC S9(9)  VALUE ZERO COMP-3.            
011191     03  W-IDARTNR-L8-X.                                                  
011192         05  W-IDARTNR-L8        PIC S9(9)  VALUE ZERO COMP-3.            
011193     03  W-TIAAAA-L8-X.                                                   
011194         05  W-TIAAAA-L8         PIC  9(4)  VALUE ZERO.                   
011200                                                                          
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011710     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800                                                                          
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100                                                                          
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012410                                                                          
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012800     EJECT                                                                
012810                                                                          
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
013102 01  DLI-IO-WDA901.                                                       
013103*    03  -COPY WDA901                                                     
013104     EJECT                                                                
013105                                                                          
013106 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
013107 01  DLI-IO-WDA911.                                                       
013108*    03  -COPY WDA911                                                     
013109     EJECT                                                                
013110                                                                          
013111 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA912'.                      
013112 01  DLI-IO-WDA912.                                                       
013120*    03  -COPY WDA912                                                     
013400     EJECT                                                                
013410                                                                          
013420 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA921'.                      
013430 01  DLI-IO-WDA921.                                                       
013440*    03  -COPY WDA921                                                     
013450     EJECT                                                                
013456                                                                          
013460 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
013470 01  DLI-IO-WDD311.                                                       
013480*    03  -COPY WDD311                                                     
013490     EJECT                                                                
013492                                                                          
013493 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013494 01  DLI-IO-WDK601.                                                       
013495*    03  -COPY WDK601                                                     
013496     EJECT                                                                
013497                                                                          
013498 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL801'.                      
013499 01  DLI-IO-WDL801.                                                       
013500*    03  -COPY WDL801                                                     
013501     EJECT                                                                
013502                                                                          
013503 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL811'.                      
013504 01  DLI-IO-WDL811.                                                       
013505*    03  -COPY WDL811                                                     
013506     EJECT                                                                
013507                                                                          
013510 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013610                                                                          
013700*01  -COPY W0008   -PRE USEA-                                             
013800     05  FILLER                  PIC X.                                   
013901                                                                          
013902*01  -COPY W0008  -PRE WDA9-                                              
013910     05  FILLER                  PIC X.                                   
014100                                                                          
014101*01  -COPY W0008  -PRE WDA9B-                                             
014102     05  FILLER                  PIC X.                                   
014103                                                                          
014104*01  -COPY W0008  -PRE WDD3B-                                             
014105     05  FILLER                  PIC X.                                   
014106                                                                          
014107*01  -COPY W0008  -PRE WDK6-                                              
014108     05  FILLER                  PIC X.                                   
014109                                                                          
014110*01  -COPY W0008  -PRE WDL8-                                              
014111     05  FILLER                  PIC X.                                   
014112     EJECT                                                                
014113                                                                          
014114 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
014115                           WDA9-PCB WDA9B-PCB                             
014116                           WDD3B-PCB                                      
014117                           WDK6-PCB                                       
014118                           WDL8-PCB.                                      
014119 MAIN SECTION.                                                            
014120     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
014121                           WDA9-PCB WDA9B-PCB                             
014130                           WDD3B-PCB                                      
014140                           WDK6-PCB                                       
014150                           WDL8-PCB.                                      
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-KOLLA-NYCKLAR                                            
014800       IF NYCKLAR-OK                                                      
015300         PERFORM F-LAES-VISA-INFO                                         
015400       END-IF                                                             
015700       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O16801 + 4                      
015800       PERFORM IMS-INSERT-MSG                                             
015900     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016510                                                                          
016600 A-INIT SECTION.                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16801                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I16801                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
017900     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
018000     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
018100                                                                          
018200     MOVE LOW-VALUE        TO MSG-AREA                                    
018300     MOVE 'W3O168N1'       TO MFS-IDMOD                                   
018400     MOVE '3168'           TO MOD-IDTRANS                                 
018500     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
018600                                                                          
018700     IF EGEN-MID OR HELP-MID                                              
018800       CONTINUE                                                           
018900     ELSE                                                                 
019000       MOVE SPACE          TO MFS-KDTRTYP                                 
019100       MOVE '7'            TO MFS-IDPFK                                   
019200     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019800                                                                          
019810 B-KOLLA-NYCKLAR SECTION.                                                 
019900     MOVE ALL '+'             TO MSGI-WMSGINIT                            
020000     MOVE '001'               TO MSGI-KDCALL                              
020100     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
020200     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
020300     MOVE '3168'              TO MSGI-IDTRANS                             
020400     IF GODK-MID                                                          
020501       MOVE MID-IDFKNGRP-IN   TO MSGI-IDFKNGRP                            
020504       MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
020510       MOVE MID-TIAAPP-FOM-IN TO MSGI-TIAAVV-FOM                          
020520       MOVE MID-TIAAPP-TOM-IN TO MSGI-TIAAVV-TOM                          
020600     END-IF                                                               
020601*    IF MID-IDFKNGRP-IN = ALL '+'                                         
020602*      MOVE MID-IDFKNGRP-UT   TO MSGI-IDFKNGRP                            
020603*    ELSE                                                                 
020604*      MOVE MID-IDFKNGRP-IN   TO MSGI-IDFKNGRP                            
020605*    END-IF                                                               
020610*    IF MID-IDARTNR-IN = ALL '+'                                          
020620*      MOVE MID-IDARTNR-UT    TO MSGI-IDARTNR                             
020630*    ELSE                                                                 
020640*      MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
020650*    END-IF                                                               
020662     IF MID-TIAAPP-FOM-IN = ALL '+'                                       
020663       MOVE MID-TIAAPP-FOM-UT TO MSGI-TIAAVV-FOM                          
020664     ELSE                                                                 
020665       MOVE MID-TIAAPP-FOM-IN TO MSGI-TIAAVV-FOM                          
020666     END-IF                                                               
020667     IF MID-TIAAPP-TOM-IN = ALL '+'                                       
020668       MOVE MID-TIAAPP-TOM-UT TO MSGI-TIAAVV-TOM                          
020669     ELSE                                                                 
020670       MOVE MID-TIAAPP-TOM-IN TO MSGI-TIAAVV-TOM                          
020671     END-IF                                                               
020680                                                                          
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020800                                                                          
020900     MOVE JA TO NYCKLAR-SW                                                
021101                                                                          
021111*    -- KONTROLL AV FUNKTIONSGRUPP                                        
021112     MOVE MFS-RENSA-FAELT     TO MOD-IDFKNGRP-IN                          
021113                                                                          
021114     IF MID-IDFKNGRP-IN NOT = ALL '+'                                     
021115       MOVE '7'               TO MFS-IDPFK                                
021116       MOVE SPACE             TO MFS-KDTRTYP                              
021117     END-IF                                                               
021118     INSPECT MSGI-IDFKNGRP REPLACING LEADING SPACE BY ZERO                
021119     IF MSGI-IDFKNGRP NUMERIC                                             
021120       MOVE MSGI-IDFKNGRP     TO W-IDFKNGRP-A9B                           
021123                                 WS-IDFKNGRP-JFR                          
021124       IF WS-IDFKNGRP-JFR > ZERO                                          
021125         MOVE JA              TO SOK-FKNGRP-SW                            
021126       END-IF                                                             
021127     ELSE                                                                 
021128       MOVE NEJ               TO NYCKLAR-SW                               
021129     END-IF                                                               
021130                                                                          
021135*    -- KONTROLL AV ARTIKEL                                               
021136     MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN                           
021137                                                                          
021138     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021139       MOVE '7'               TO MFS-IDPFK                                
021140       MOVE SPACE             TO MFS-KDTRTYP                              
021141     END-IF                                                               
021142     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021146     IF MSGI-IDARTNR NUMERIC                                              
021147       MOVE MSGI-IDARTNR      TO TEST-IDARTNR                             
021149       IF BYT03-OBJEKT OR                                                 
021150         MSGI-IDARTNR = ZERO                                              
021152         MOVE MSGI-IDARTNR    TO W-IDARTNR-A9                             
021153                                 W-IDARTNR-D3                             
021154                                 W-IDARTNR-K6                             
021155                                 W-IDARTNR-L8                             
021156                                 WS-IDARTNR-JFR                           
021157         IF WS-IDARTNR-JFR > ZERO                                         
021158           MOVE NEJ           TO SOK-FKNGRP-SW                            
021159         END-IF                                                           
021160       ELSE                                                               
021161         MOVE NEJ             TO NYCKLAR-SW                               
021162       END-IF                                                             
021163     ELSE                                                                 
021164       MOVE NEJ               TO NYCKLAR-SW                               
021165     END-IF                                                               
021166                                                                          
021167*    -- KONTROLL AV PERIOD-FOM                                            
021168     MOVE MFS-RENSA-FAELT     TO MOD-TIAAPP-FOM-IN                        
021169                                                                          
021170     IF MID-TIAAPP-FOM-IN NOT = ALL '+'                                   
021171       MOVE '7'               TO MFS-IDPFK                                
021172       MOVE SPACE             TO MFS-KDTRTYP                              
021173     END-IF                                                               
021174     INSPECT MSGI-TIAAVV-FOM REPLACING LEADING SPACE BY ZERO              
021175     IF MSGI-TIAAVV-FOM NUMERIC                                           
021176       MOVE MSGI-TIAAVV-FOM   TO WS-TIAAPP-FOM                            
021177                                 WS-TIAAPP-FOM-ALFA                       
021178       MOVE 200000            TO WS-DAAAPP-FOM                            
021179       ADD WS-TIAAPP-FOM      TO WS-DAAAPP-FOM                            
021180       MOVE WS-DAAAPP-FOM     TO W-DAAAPP-A9-MIN                          
021181     ELSE                                                                 
021182       MOVE NEJ               TO NYCKLAR-SW                               
021190     END-IF                                                               
021201                                                                          
021202*    -- KONTROLL AV PERIOD-TOM                                            
021203     MOVE MFS-RENSA-FAELT     TO MOD-TIAAPP-TOM-IN                        
021204                                                                          
021205     IF MID-TIAAPP-TOM-IN NOT = ALL '+'                                   
021206       MOVE '7'               TO MFS-IDPFK                                
021207       MOVE SPACE             TO MFS-KDTRTYP                              
021208     END-IF                                                               
021209     INSPECT MSGI-TIAAVV-TOM REPLACING LEADING SPACE BY ZERO              
021210     IF MSGI-TIAAVV-TOM NUMERIC                                           
021211       MOVE MSGI-TIAAVV-TOM   TO WS-TIAAPP-TOM                            
021212                                 WS-TIAAPP-TOM-ALFA                       
021213       MOVE 200000            TO WS-DAAAPP-FOM                            
021214       ADD WS-TIAAPP-TOM      TO WS-DAAAPP-TOM                            
021215       MOVE WS-DAAAPP-TOM     TO W-DAAAPP-A9-MAX                          
021216     ELSE                                                                 
021217       MOVE NEJ               TO NYCKLAR-SW                               
021218     END-IF                                                               
021219                                                                          
021220*    -- SAMBANDSKONTROLL AV FUNKTIONSGRUPP -> ARTIKEL                     
021221     IF NYCKLAR-OK                                                        
021222       IF (WS-IDFKNGRP-JFR > ZERO AND                                     
021224           WS-IDARTNR-JFR  > ZERO)      OR                                
021225          (WS-IDFKNGRP-JFR = ZERO AND                                     
021226           WS-IDARTNR-JFR  = ZERO)                                        
021227         MOVE NEJ             TO NYCKLAR-SW                               
021229       END-IF                                                             
021230     END-IF                                                               
021300                                                                          
021301*    -- SAMBANDSKONTROLL MM AV PERIOD-FROM <-> PERIOD-TOM                 
021302     IF NYCKLAR-OK                                                        
021306       IF WS-TIAAPP-TOM < WS-TIAAPP-FOM                                   
021307         MOVE NEJ             TO NYCKLAR-SW                               
021308       END-IF                                                             
021309       IF (WS-TIAAPP-FOM-2-4 = '00' OR > '12') OR                         
021310          (WS-TIAAPP-TOM-2-4 = '00' OR > '12')                            
021311         MOVE NEJ             TO NYCKLAR-SW                               
021312       END-IF                                                             
021313     END-IF                                                               
021314                                                                          
021315     IF GODK-MID OR NYCKLAR-OK                                            
021320       MOVE MSGI-IDFKNGRP     TO MOD-IDFKNGRP-UT                          
021321       INSPECT MOD-IDFKNGRP-UT REPLACING LEADING ZERO BY SPACE            
021330       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                           
021331       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021340       MOVE MSGI-TIAAVV-FOM   TO MOD-TIAAPP-FOM-UT                        
021350       MOVE MSGI-TIAAVV-TOM   TO MOD-TIAAPP-TOM-UT                        
021360     ELSE                                                                 
021370       MOVE MFS-RENSA-FAELT   TO MOD-IDFKNGRP-UT                          
021380                                 MOD-IDARTNR-UT                           
021390                                 MOD-TIAAPP-FOM-UT                        
021391                                 MOD-TIAAPP-TOM-UT                        
021392     END-IF                                                               
021393                                                                          
021400     IF NYCKLAR-FEL                                                       
021500       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
021600       CALL WMEDKONV USING MED-WMEDAREA                                   
021700       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
021800       PERFORM MFS-RENSA-FAELT-IN                                         
021900       PERFORM MFS-RENSA-FAELT-UT                                         
022000     END-IF                                                               
022100     .                                                                    
022300     EJECT                                                                
022600                                                                          
022610 F-LAES-VISA-INFO SECTION.                                                
022700     IF SOK-FKNGRP                                                        
022710       MOVE SPACE             TO MOD-BEART-CORE                           
022711       PERFORM IMS-GN-WDA901-BSEQ                                         
022712       IF SEGMENT-FINNS                                                   
022713         MOVE NEJ             TO SOK-RECORDS-SW                           
022714       END-IF                                                             
022715       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
022716                     SEGMENT-SLUT                                         
022717         MOVE UPB-IDARTNR     TO W-IDARTNR-A9                             
022718         PERFORM IMS-GU-WDA901                                            
022723         PERFORM IMS-GNP-WDA912                                           
022730         PERFORM UNTIL SEGMENT-SAKNAS                                     
022750           PERFORM FA-BYGG-WDA912                                         
022760           PERFORM IMS-GNP-WDA912                                         
022770         END-PERFORM                                                      
022771                                                                          
022772         PERFORM IMS-GU-WDA901                                            
022782         PERFORM IMS-GNP-WDA911                                           
022790         PERFORM UNTIL SEGMENT-SAKNAS                                     
022791           PERFORM IMS-GNP-WDA921                                         
022792           PERFORM UNTIL SEGMENT-SAKNAS                                   
022793             PERFORM FB-BYGG-WDA921                                       
022794             PERFORM IMS-GNP-WDA921                                       
022795           END-PERFORM                                                    
022796           PERFORM IMS-GNP-WDA911                                         
022797         END-PERFORM                                                      
022798         PERFORM IMS-GN-WDA901-BSEQ                                       
022799       END-PERFORM                                                        
022800     END-IF                                                               
022810                                                                          
022820     IF SOK-ARTNR                                                         
022821       PERFORM IMS-GU-WDD311-BSEQ                                         
022822       IF SEGMENT-FINNS                                                   
022823         MOVE TEXT-BEART       TO MOD-BEART-CORE                          
022824       ELSE                                                               
022825         MOVE 'UNKNOWN'        TO MOD-BEART-CORE                          
022826       END-IF                                                             
022827                                                                          
022828       PERFORM IMS-GU-WDA901                                              
022829       IF SEGMENT-FINNS                                                   
022831         MOVE NEJ              TO SOK-RECORDS-SW                          
022833         PERFORM IMS-GNP-WDA912                                           
022834         PERFORM UNTIL SEGMENT-SAKNAS                                     
022835           PERFORM FA-BYGG-WDA912                                         
022836           PERFORM IMS-GNP-WDA912                                         
022837         END-PERFORM                                                      
022838                                                                          
022839         PERFORM IMS-GU-WDA901                                            
022840         PERFORM IMS-GNP-WDA911                                           
022841         PERFORM UNTIL SEGMENT-SAKNAS                                     
022842           PERFORM IMS-GNP-WDA921                                         
022843           PERFORM UNTIL SEGMENT-SAKNAS                                   
022844             PERFORM FB-BYGG-WDA921                                       
022845             PERFORM IMS-GNP-WDA921                                       
022846           END-PERFORM                                                    
022847           PERFORM IMS-GNP-WDA911                                         
022848         END-PERFORM                                                      
022849       END-IF                                                             
022850     END-IF                                                               
022860                                                                          
022900     IF RECORDS-MISSING                                                   
023010       MOVE ERR-INFO-MISSING     TO MED-IDMFSFEL                          
023100       CALL WMEDKONV USING MED-WMEDAREA                                   
023200       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
023300*      PERFORM MFS-RENSA-FAELT-UT                                         
023400     ELSE                                                                 
023500       PERFORM FC-BYGG-MOD                                                
023600     END-IF                                                               
023601                                                                          
023610     MOVE '002'                  TO MSGI-KDCALL                           
023640     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023700     .                                                                    
023800     EJECT                                                                
023810                                                                          
023900 FA-BYGG-WDA912 SECTION.                                                  
024000     COMPUTE WS-SUINVEST-DC = WS-SUINVEST-DC +                            
024100                              UPA-SUINVEST-DC                             
024200     END-COMPUTE                                                          
024300     COMPUTE WS-SULEVANT-DC = WS-SULEVANT-DC +                            
024400                              UPA-SULEVANT-DC                             
024410     END-COMPUTE                                                          
024500     .                                                                    
024800     EJECT                                                                
024810                                                                          
024820 FB-BYGG-WDA921 SECTION.                                                  
024821     COMPUTE WS-SUINVEST-REM = WS-SUINVEST-REM   +                        
024822                               UPP-SUINVEST-REM                           
024824     END-COMPUTE                                                          
024840     .                                                                    
024850     EJECT                                                                
024860                                                                          
024870 FC-BYGG-MOD SECTION.                                                     
024880     COMPUTE WS-SUINVEST-TOT = WS-SUINVEST-DC +                           
024881                               WS-SUINVEST-REM                            
024882     END-COMPUTE                                                          
024883* ??????????????                                                          
024884     MOVE ZERO               TO MOD-RECOST                                
024886     MOVE WS-SUINVEST-DC     TO MOD-SUINVEST-DC                           
024887     MOVE WS-SUINVEST-REM    TO MOD-SUINVEST-REM                          
024888     MOVE WS-SUINVEST-TOT    TO MOD-SUINVEST-TOT                          
024889     MOVE WS-SULEVANT-DC     TO MOD-SULEVANT-DC                           
024891                                                                          
024892     IF WS-SULEVANT-DC > ZERO                                             
024893       COMPUTE WS-REPROCENT ROUNDED = (WS-SUINVEST-TOT /                  
024894                                       WS-SULEVANT-DC) *                  
024895                                       100                                
024896       END-COMPUTE                                                        
024897       MOVE WS-REPROCENT     TO MOD-REPROCENT                             
024898     ELSE                                                                 
024899       MOVE ZERO             TO MOD-REPROCENT                             
024900     END-IF                                                               
024901                                                                          
024910     PERFORM IMS-GU-WDK601                                                
024911     IF ART-IDLEVNR NOT = 8261 AND 0000                                   
024912       PERFORM FCA-BER-COREINVEST                                         
024913       MOVE WS-SUINVEST-CORE TO MOD-SUINVEST-CORE                         
024914     ELSE                                                                 
024915       MOVE ZERO             TO MOD-SUINVEST-CORE                         
024916     END-IF                                                               
024917     .                                                                    
024918     EJECT                                                                
024920                                                                          
024930 FCA-BER-COREINVEST SECTION.                                              
025012     MOVE MSGI-TIAAVV-FOM(1:2)   TO WS-TIAA-FOM                           
025013     MOVE MSGI-TIAAVV-TOM(1:2)   TO WS-TIAA-TOM                           
025014                                                                          
025015     IF WS-TIAA-FOM < WS-TIAA-TOM                                         
025017       MOVE 'AAPP'               TO DAT-KDDATFORM                         
025018       MOVE MSGI-TIAAVV-FOM      TO DAT-I-TIDATUM                         
025019                                                                          
025020       CALL WDATKONV USING DAT-KDDATFORM                                  
025021                           DAT-I-TIDATUM                                  
025022                           DAT-O-TIDATUM                                  
025023                           DAT-KDSVAR                                     
025024                                                                          
025025       IF DAT-KDSVAR = ' '                                                
025026         MOVE DAT-TIVV           TO INDX                                  
025027       ELSE                                                               
025028         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
025029         MOVE +1000              TO RKOD-ABEND-MED-DUMP                   
025030         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
025031       END-IF                                                             
025032                                                                          
025033       COMPUTE W-TIAAAA-L8 = WS-TIAAAA-FOM +                              
025034                             WS-TIAA-FOM                                  
025035       END-COMPUTE                                                        
025036       PERFORM IMS-GU-WDL811                                              
025037                                                                          
025039       PERFORM UNTIL INDX > MAX-INDX                                      
025040         ADD AAR-KVOI-SDC (INDX) TO WS-SUINVEST-CORE                      
025041         ADD AAR-KVOI-NDC (INDX) TO WS-SUINVEST-CORE                      
025042         ADD +1 TO INDX                                                   
025043       END-PERFORM                                                        
025045                                                                          
025047       MOVE 'AAPP'               TO DAT-KDDATFORM                         
025048       MOVE MSGI-TIAAVV-TOM      TO DAT-I-TIDATUM                         
025049                                                                          
025050       CALL WDATKONV USING DAT-KDDATFORM                                  
025051                           DAT-I-TIDATUM                                  
025052                           DAT-O-TIDATUM                                  
025053                           DAT-KDSVAR                                     
025054                                                                          
025055       IF DAT-KDSVAR = ' '                                                
025056         MOVE DAT-TIVV           TO MAX-INDX                              
025057       ELSE                                                               
025058         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
025059         MOVE +1000              TO RKOD-ABEND-MED-DUMP                   
025060         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
025061       END-IF                                                             
025062                                                                          
025063       COMPUTE W-TIAAAA-L8 = WS-TIAAAA-TOM +                              
025064                             WS-TIAA-TOM                                  
025065       END-COMPUTE                                                        
025066       PERFORM IMS-GU-WDL811                                              
025067                                                                          
025069       MOVE +1 TO INDX                                                    
025070       PERFORM UNTIL INDX > MAX-INDX                                      
025071         ADD AAR-KVOI-SDC (INDX) TO WS-SUINVEST-CORE                      
025072         ADD AAR-KVOI-NDC (INDX) TO WS-SUINVEST-CORE                      
025073         ADD +1 TO INDX                                                   
025074       END-PERFORM                                                        
025075     END-IF                                                               
025076                                                                          
025077     IF WS-TIAA-FOM = WS-TIAA-TOM                                         
025078       MOVE 'AAPP'               TO DAT-KDDATFORM                         
025079       MOVE MSGI-TIAAVV-FOM      TO DAT-I-TIDATUM                         
025080                                                                          
025081       CALL WDATKONV USING DAT-KDDATFORM                                  
025082                           DAT-I-TIDATUM                                  
025083                           DAT-O-TIDATUM                                  
025084                           DAT-KDSVAR                                     
025085                                                                          
025086       IF DAT-KDSVAR = ' '                                                
025087         MOVE DAT-TIVV           TO INDX                                  
025088       ELSE                                                               
025089         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
025090         MOVE +1000              TO RKOD-ABEND-MED-DUMP                   
025091         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
025092       END-IF                                                             
025093                                                                          
025094       MOVE 'AAPP'               TO DAT-KDDATFORM                         
025095       MOVE MSGI-TIAAVV-TOM      TO DAT-I-TIDATUM                         
025096                                                                          
025097       CALL WDATKONV USING DAT-KDDATFORM                                  
025098                           DAT-I-TIDATUM                                  
025099                           DAT-O-TIDATUM                                  
025100                           DAT-KDSVAR                                     
025101                                                                          
025102       IF DAT-KDSVAR = ' '                                                
025103         MOVE DAT-TIVV           TO MAX-INDX                              
025104       ELSE                                                               
025105         DISPLAY '**** TIAAVV' DAT-I-TIDATUM                              
025106         MOVE +1000              TO RKOD-ABEND-MED-DUMP                   
025107         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
025108       END-IF                                                             
025111                                                                          
025112       COMPUTE W-TIAAAA-L8 = WS-TIAAAA-FOM +                              
025113                             WS-TIAA-FOM                                  
025114       END-COMPUTE                                                        
025115       PERFORM IMS-GU-WDL811                                              
025116                                                                          
025119       PERFORM UNTIL INDX > MAX-INDX                                      
025120         ADD AAR-KVOI-SDC (INDX) TO WS-SUINVEST-CORE                      
025121         ADD AAR-KVOI-NDC (INDX) TO WS-SUINVEST-CORE                      
025122         ADD +1 TO INDX                                                   
025123       END-PERFORM                                                        
025124     END-IF                                                               
025125     .                                                                    
025126     EJECT                                                                
025127                                                                          
025128 MFS-RENSA-FAELT-UT SECTION.                                              
025130*    --- ALLA UTDATA-FÄLT                                                 
025200     MOVE MFS-RENSA-FAELT TO MOD-BEART-CORE                               
025210                             MOD-RECOST                                   
025211                             MOD-SUINVEST-DC                              
025220                             MOD-SUINVEST-REM                             
025230                             MOD-SUINVEST-TOT                             
025231                             MOD-SUINVEST-CORE                            
025240                             MOD-SULEVANT-DC                              
025250                             MOD-REPROCENT                                
025500     .                                                                    
025600     EJECT                                                                
025700                                                                          
025800 MFS-RENSA-FAELT-IN SECTION.                                              
026000*    --- ALLA INDATA-FÄLT                                                 
026100     MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-IN                              
026200                             MOD-IDARTNR-IN                               
026210                             MOD-TIAAPP-FOM-IN                            
026220                             MOD-TIAAPP-TOM-IN                            
026300     .                                                                    
028000     EJECT                                                                
028010                                                                          
029500* --- IMS SEKTIONER ---                                                   
029700 IMS-GET-MSG SECTION.                                                     
029900     MOVE '  QC'          TO GODK-STATUSKODER                             
030000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400                                                                          
030500 IMS-INSERT-MSG SECTION.                                                  
030910     IF MSGI-IDLAND-SPR = 'GB'                                            
030920       MOVE 'N'           TO MFS-KDHUVOMR                                 
030930     END-IF                                                               
031000     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
031100     MOVE SPACE           TO GODK-STATUSKODER                             
031200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031601     EJECT                                                                
031602                                                                          
031616 IMS-GN-WDA901-BSEQ SECTION.                                              
031617     STRING 'WDA901  (WDA9BSEQ =' W-WDA9BSEQ-X ')'                        
031619     DELIMITED BY SIZE INTO SSA1                                          
031620     MOVE '  GEGB'          TO GODK-STATUSKODER                           
031621     CALL CBLTDLI USING GN WDA9B-PCB DLI-IO-WDA901 SSA1                   
031622     MOVE WDA9B-STATUS-CODE TO STATUS-WS                                  
031623     PERFORM IMS-STATUSKONTROLL                                           
031624     .                                                                    
031657     EJECT                                                                
031658                                                                          
031659 IMS-GU-WDA901 SECTION.                                                   
031660     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-A9-X ')'                      
031661          DELIMITED BY SIZE INTO SSA1                                     
031662     MOVE '  GE'           TO GODK-STATUSKODER                            
031663     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
031664     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
031665     PERFORM IMS-STATUSKONTROLL                                           
031666     .                                                                    
031710                                                                          
031711 IMS-GNP-WDA912 SECTION.                                                  
031712     STRING 'WDA912  (DAAAPPR >=' W-DAAAPP-A9-MIN-X                       
031713                    '&DAAAPPR <=' W-DAAAPP-A9-MAX-X ')'                   
031714          DELIMITED BY SIZE INTO SSA1                                     
031715     MOVE '  GE'           TO GODK-STATUSKODER                            
031716     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA912 SSA1                   
031717     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
031718     PERFORM IMS-STATUSKONTROLL                                           
031719     .                                                                    
031720                                                                          
031721 IMS-GNP-WDA911 SECTION.                                                  
031722     MOVE 'WDA911  '       TO SSA1                                        
031723     MOVE '  GE'           TO GODK-STATUSKODER                            
031724     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
031725     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
031726     PERFORM IMS-STATUSKONTROLL                                           
031727     .                                                                    
031728                                                                          
031729 IMS-GNP-WDA921 SECTION.                                                  
031730     STRING 'WDA921  (DAAAPP  >=' W-DAAAPP-A9-MIN-X                       
031731                    '&DAAAPP  <=' W-DAAAPP-A9-MAX-X ')'                   
031732          DELIMITED BY SIZE INTO SSA1                                     
031733     MOVE '  GE'            TO GODK-STATUSKODER                           
031734     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA921 SSA1                   
031735     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
031736     PERFORM IMS-STATUSKONTROLL                                           
031737     .                                                                    
031738     EJECT                                                                
031739                                                                          
031740 IMS-GU-WDD311-BSEQ SECTION.                                              
031741     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-D3-X ')'                      
031742     DELIMITED BY SIZE INTO SSA1                                          
031750     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-D3-X ')'                      
031760     DELIMITED BY SIZE INTO SSA2                                          
031770     MOVE '  GE'            TO GODK-STATUSKODER                           
031780     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
031790     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
031791     PERFORM IMS-STATUSKONTROLL                                           
031792     .                                                                    
031793     EJECT                                                                
031794                                                                          
031795 IMS-GU-WDK601 SECTION.                                                   
031796     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
031797          DELIMITED BY SIZE INTO SSA1                                     
031800     MOVE '  '             TO GODK-STATUSKODER                            
031810     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
031820     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031830     PERFORM IMS-STATUSKONTROLL                                           
031840     .                                                                    
031850     EJECT                                                                
031860                                                                          
031870 IMS-GU-WDL811  SECTION.                                                  
031880                                                                          
031890     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-L8-X ')'                      
031891          DELIMITED BY SIZE INTO SSA1                                     
031892     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-L8-X ')'                       
031893          DELIMITED BY SIZE INTO SSA2                                     
031894     MOVE '  GE' TO GODK-STATUSKODER                                      
031895     CALL CBLTDLI USING GU WDL8-PCB                                       
031896                                 DLI-IO-WDL811 SSA1 SSA2                  
031897     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
031898     PERFORM IMS-STATUSKONTROLL                                           
031899     .                                                                    
031900     EJECT                                                                
031901                                                                          
031910 IMS-STATUSKONTROLL SECTION.                                              
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GODK-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400         DELIMITED BY SIZE INTO FELTEXT                                   
032500         CALL FELLOG                                                      
032600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032700         CONTINUE                                                         
032800     END-SEARCH                                                           
032900     .                                                                    
