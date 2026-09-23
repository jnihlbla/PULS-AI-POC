000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6021700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/04/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700**   FUNKTION:                                                            
000800*        REGISTRERING AV INTERVALL ARTIKELNR/ LEV.NR /                    
000900*        FUNKTIONSGRUPP PER KDARBTYP OCH IDPERSON.                        
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDP3                                       
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*                              WDK6B                                      
001400*                              WDF1                                       
001500*                              WDP3A                                      
001600*                              WDP3B                                      
001700*                              WDP3C                                      
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T217                                              
002100*        MID:         W6I21701                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O21701                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(08)   VALUE 'W6021700'.            
003400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  BORTTAG                     PIC X       VALUE 'B'.                   
003800 77  NYUPPLAGG                   PIC X       VALUE 'N'.                   
003900 77  ARTNR                       PIC X       VALUE 'A'.                   
004000 77  LEVNR                       PIC X       VALUE 'L'.                   
004100 77  FKNGRP                      PIC X       VALUE 'F'.                   
004200 77  WS-IDPERSON-QUAL            PIC X(3)    VALUE SPACE.                 
004300 77  WS-IDPERSON-PACK            PIC X(3)    VALUE SPACE.                 
004310 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
004400 77  WS-KDARBVAL                 PIC X       VALUE SPACE.                 
004500 77  WS-SISTA-SIDAN              PIC X       VALUE SPACE.                 
004600 77  WS-SPARAD-IDPERSON          PIC 9(3)    VALUE ZERO.                  
004700 77  IX                          PIC S9(3)   VALUE +0    COMP-3.          
004800 77  IX-MAX                      PIC S9(3)   VALUE +12   COMP-3.          
004900                                                                          
005000 01  WS-IDPERSON-QUAL-NY         PIC X(3)    VALUE SPACE.                 
005100 01  WS-IDPERSON-PACK-NY         PIC X(3)    VALUE SPACE.                 
005200 01  WS-IDARTNR-FOM              PIC X(9)    VALUE SPACE.                 
005300 01  WS-IDARTNR-FOM-NUM REDEFINES WS-IDARTNR-FOM PIC 9(9).                
005400 01  WS-IDARTNR-TOM              PIC X(9)    VALUE SPACE.                 
005500 01  WS-IDARTNR-TOM-NUM REDEFINES WS-IDARTNR-TOM PIC 9(9).                
005600 01  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005700 01  WS-IDFKNGRP-FOM             PIC X(4)    VALUE SPACE.                 
005800 01  WS-IDFKNGRP-FOM-NUM REDEFINES WS-IDFKNGRP-FOM PIC 9(4).              
005900 01  WS-IDFKNGRP-TOM             PIC X(4)    VALUE SPACE.                 
006000 01  WS-IDFKNGRP-TOM-NUM REDEFINES WS-IDFKNGRP-TOM PIC 9(4).              
006100     EJECT                                                                
006200 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
006300 77  ART-SW                      PIC X       VALUE 'N'.                   
006400 77  LEV-SW                      PIC X       VALUE 'N'.                   
006500 77  FKN-SW                      PIC X       VALUE 'N'.                   
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200 77  INPUT-SW                    PIC X       VALUE 'J'.                   
007300     88  INPUT-FINNS                         VALUE 'J'.                   
007400     88  INPUT-SAKNAS                        VALUE 'N'.                   
007500 77  UPPDATERA-SW                PIC X.                                   
007600     88  UPPDATERA-BORTTAG                   VALUE 'B'.                   
007700     88  UPPDATERA-NYUPPLAGG                 VALUE 'N'.                   
007800 77  INTERVALL-SW                PIC X       VALUE SPACE.                 
007900     88  INTERVALL-ARTNR                     VALUE 'A'.                   
008000     88  INTERVALL-LEVNR                     VALUE 'L'.                   
008100     88  INTERVALL-FKNGRP                    VALUE 'F'.                   
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '6217'.                
008400     88  GODK-MID                            VALUE '6217'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     EJECT                                                                
009210*01 -COPY WWLNDKON                                                        
009300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009400*01 -COPY WMEDAREA                                                        
009500     EJECT                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010300     03  INF-LAST-PAGE           PIC X(3)    VALUE '115'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
010600     03  INFORMATION-MISSING     PIC X(3)    VALUE '760'.                 
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011100*01 -COPY WMSGINIT                                                        
011200     EJECT                                                                
011300 01  SPAR-AREA.                                                           
011400     03  SPAR-IDTRANS            PIC X(4)  VALUE '6217'.                  
011500     03  SPAR-KDARBTYP-ENTER     PIC X(8).                                
011600     03  SPAR-KDARBTYP-NEXT      PIC X(8).                                
011700     03  SPAR-IDPERSON-ENTER     PIC 9(3).                                
011800     03  SPAR-IDPERSON-NEXT      PIC 9(3).                                
011900     03  SPAR-IDARTNR-FOM-ENTER  PIC 9(9).                                
012000     03  SPAR-IDARTNR-TOM-ENTER  PIC 9(9).                                
012010     03  SPAR-IDLANDX2-ENTER     PIC X(2).                                
012100     03  SPAR-IDARTNR-FOM-NEXT   PIC 9(9).                                
012200     03  SPAR-IDARTNR-TOM-NEXT   PIC 9(9).                                
012300     03  SPAR-IDLEVNR-ENTER      PIC X(5).                                
012400     03  SPAR-IDLEVNR-NEXT       PIC X(5).                                
012500     03  SPAR-IDFKNGRP-FOM-ENTER PIC 9(4).                                
012600     03  SPAR-IDFKNGRP-TOM-ENTER PIC 9(4).                                
012700     03  SPAR-IDFKNGRP-FOM-NEXT  PIC 9(4).                                
012800     03  SPAR-IDFKNGRP-TOM-NEXT  PIC 9(4).                                
012810     03  SPAR-IDLANDX2-NEXT      PIC X(2).                                
012900     EJECT                                                                
013000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013200     SKIP3                                                                
013300*01  MID -COPY W6I21701                                                   
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013600*01  -COPY WMSGAREA                                                       
013700     EJECT                                                                
013800     03  MOD REDEFINES MSG-AREA.                                          
013900*      05  -COPY W6O21701                                                 
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014200*01  -COPY WMFSAREA                                                       
014300     EJECT                                                                
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014600     SKIP3                                                                
014700 01  NYCKLAR-TILL-DLI.                                                    
014800     03  W-KDARBTYP-X.                                                    
014900         05  W-KDARBTYP          PIC X(8)   VALUE SPACE.                  
015000     03  W-IDPERSON-X.                                                    
015100         05  W-IDPERSON          PIC S9(3)  VALUE ZERO COMP-3.            
015200     03  W-IDARTNR-X.                                                     
015300         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
015600     03  W-IDLAND-X.                                                      
015700         05  W-IDLAND            PIC  X(2)  VALUE SPACE.                  
015710     03  W-WDP321KY-MIN-X.                                                
015720         05  W-IDLAND21-MIN      PIC  X(2)  VALUE SPACE.                  
015800         05  W-IDARTNRF-MIN      PIC S9(9)  VALUE ZERO COMP-3.            
015900         05  W-IDARTNRT-MIN      PIC S9(9)  VALUE ZERO COMP-3.            
016000     03  W-WDP321KY-MAX-X.                                                
016010         05  W-IDLAND21-MAX      PIC  X(2)  VALUE HIGH-VALUE.             
016100         05  W-IDARTNRF-MAX      PIC S9(9)  VALUE +999999999              
016200                                               COMP-3.                    
016300         05  W-IDARTNRT-MAX      PIC S9(9)  VALUE +999999999              
016400                                               COMP-3.                    
016410     03  W-WDP322KY-MIN-X.                                                
016411         05  W-IDLAND22-MIN      PIC  X(2)  VALUE SPACE.                  
016420         05  W-IDLEVNR22-MIN     PIC  X(5)  VALUE SPACE.                  
016430     03  W-WDP322KY-MAX-X.                                                
016440         05  W-IDLAND22-MAX      PIC  X(2)  VALUE HIGH-VALUE.             
016450         05  W-IDLEVNR22-MAX     PIC  X(5)  VALUE HIGH-VALUE.             
016500     03  W-WDP323KY-MIN-X.                                                
016510         05  W-IDLAND23-MIN      PIC  X(2)  VALUE SPACE.                  
016600         05  W-IDFKNGRPF-MIN     PIC S9(5)  VALUE ZERO COMP-3.            
016700         05  W-IDFKNGRPT-MIN     PIC S9(5)  VALUE ZERO COMP-3.            
016800     03  W-WDP323KY-MAX-X.                                                
016810         05  W-IDLAND23-MAX      PIC  X(2)  VALUE HIGH-VALUE.             
016900         05  W-IDFKNGRPF-MAX     PIC S9(5)  VALUE +99999 COMP-3.          
017000         05  W-IDFKNGRPT-MAX     PIC S9(5)  VALUE +99999 COMP-3.          
017100     EJECT                                                                
017200     03  W-WDP3A1-MIN.                                                    
017300         05  W-IDLAND-A-MIN       PIC  X(2)  VALUE SPACE.                 
017310         05  W-IDARTNRF-A-MIN     PIC S9(9)  VALUE ZERO COMP-3.           
017400         05  W-IDARTNRT-A-MIN     PIC S9(9)  VALUE ZERO COMP-3.           
017500         05  W-KDARBTYP-A-MIN     PIC X(8)   VALUE SPACE.                 
017600     03  W-WDP3A1-MAX.                                                    
017610         05  W-IDLAND-A-MAX       PIC  X(2)  VALUE SPACE.                 
017700         05  W-IDARTNRF-A-MAX     PIC S9(9)  VALUE ZERO COMP-3.           
017800         05  W-IDARTNRT-A-MAX     PIC S9(9)  VALUE ZERO COMP-3.           
017900         05  W-KDARBTYP-A-MAX     PIC X(8)   VALUE SPACE.                 
018000     03  W-WDP3B1-MIN.                                                    
018010         05  W-IDLAND-B-MIN       PIC X(2)   VALUE SPACE.                 
018100         05  W-IDLEVNR-B-MIN      PIC X(5)   VALUE SPACE.                 
018200         05  W-KDARBTYP-B-MIN     PIC X(8)   VALUE SPACE.                 
018210     03  W-WDP3B1-MAX.                                                    
018220         05  W-IDLAND-B-MAX       PIC X(2)   VALUE SPACE.                 
018230         05  W-IDLEVNR-B-MAX      PIC X(5)   VALUE SPACE.                 
018240         05  W-KDARBTYP-B-MAX     PIC X(8)   VALUE SPACE.                 
018300     03  W-WDP3C1-MIN.                                                    
018310         05  W-IDLAND-C-MIN       PIC  X(2)  VALUE SPACE.                 
018400         05  W-IDFKNGRPF-C-MIN    PIC S9(5)  VALUE ZERO COMP-3.           
018500         05  W-IDFKNGRPT-C-MIN    PIC S9(5)  VALUE ZERO COMP-3.           
018600         05  W-KDARBTYP-C-MIN     PIC X(8)   VALUE SPACE.                 
018700     03  W-WDP3C1-MAX.                                                    
018710         05  W-IDLAND-C-MAX       PIC  X(2)  VALUE SPACE.                 
018800         05  W-IDFKNGRPF-C-MAX    PIC S9(5)  VALUE ZERO COMP-3.           
018900         05  W-IDFKNGRPT-C-MAX    PIC S9(5)  VALUE ZERO COMP-3.           
019000         05  W-KDARBTYP-C-MAX     PIC X(8)   VALUE SPACE.                 
019100     EJECT                                                                
019200*    --- STATUS-KOD FRÅN IMS                                              
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FINNS                       VALUE '  '.                  
019500     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
019600                                                   'GB'.                  
019700     SKIP2                                                                
019800 01  GODK-STATUSKODER.                                                    
019900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020000     SKIP3                                                                
020100 01  SSA1                        PIC X(96).                               
020200 01  SSA2                        PIC X(64).                               
020300 01  SSA3                        PIC X(64).                               
020400     EJECT                                                                
020500*    --- IMS FUNKTIONSKODER                                               
020600*01  -COPY W0003                                                          
020700     EJECT                                                                
020800*    ---  DLI INPUT-OUTPUT AREA                                           
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
021000 01  DLI-IO-WDP301.                                                       
021100*    03  -COPY WDP301                                                     
021200     EJECT                                                                
021300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
021400 01  DLI-IO-WDP311.                                                       
021500*    03  -COPY WDP311                                                     
021600     EJECT                                                                
021700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP321'.                      
021800 01  DLI-IO-WDP321.                                                       
021900*    03  -COPY WDP321                                                     
022000     EJECT                                                                
022100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP322'.                      
022200 01  DLI-IO-WDP322.                                                       
022300*    03  -COPY WDP322                                                     
022400     EJECT                                                                
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP323'.                      
022600 01  DLI-IO-WDP323.                                                       
022700*    03  -COPY WDP323                                                     
022800     EJECT                                                                
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
023000 01  DLI-IO-WDK601.                                                       
023100*    03  -COPY WDK601                                                     
023200     EJECT                                                                
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
023400 01  DLI-IO-WDF101.                                                       
023500*    03  -COPY WDF101                                                     
023600     EJECT                                                                
023700 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDP3A'.                      
023800 01  DLI-IO-WDP3A.                                                        
023900*    03  -COPY WDP3A1                                                     
024000     EJECT                                                                
024100 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDP3B'.                      
024200 01  DLI-IO-WDP3B.                                                        
024300*    03  -COPY WDP3B1                                                     
024400     EJECT                                                                
024500 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDP3C'.                      
024600 01  DLI-IO-WDP3C.                                                        
024700*    03  -COPY WDP3C1                                                     
024800     EJECT                                                                
024900 LINKAGE SECTION.                                                         
025000*01  -COPY W0009   -PRE MSG-                                              
025100     EJECT                                                                
025200*01  -COPY W0008   -PRE USEA-                                             
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008   -PRE WDP3-                                             
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008   -PRE WDP32-                                            
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008   -PRE WDK6-                                             
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400*01  -COPY W0008   -PRE WDK6B-                                            
026500     05  FILLER                  PIC X.                                   
026600     EJECT                                                                
026700*01  -COPY W0008   -PRE WDF1-                                             
026800     05  FILLER                  PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008   -PRE WDP3A-                                            
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300*01  -COPY W0008   -PRE WDP3B-                                            
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600*01  -COPY W0008   -PRE WDP3C-                                            
027700     05  FILLER                  PIC X.                                   
027800     EJECT                                                                
027900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDP3-PCB WDP32-PCB            
028000                           WDK6-PCB WDK6B-PCB WDF1-PCB WDP3A-PCB          
028100                           WDP3B-PCB WDP3C-PCB.                           
028200 MAIN SECTION.                                                            
028300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDP3-PCB WDP32-PCB            
028400                           WDK6-PCB WDK6B-PCB WDF1-PCB WDP3A-PCB          
028500                           WDP3B-PCB WDP3C-PCB.                           
028600                                                                          
028700     PERFORM IMS-GET-MSG                                                  
028800     IF SEGMENT-FINNS                                                     
028900       PERFORM A-INIT                                                     
029000       PERFORM B-KOLLA-NYCKLAR                                            
029100       IF NYCKLAR-OK                                                      
029200         IF MFS-UPDATE                                                    
029300           PERFORM G-KOLLA-INPUT                                          
029400           IF INDATA-OK                                                   
029500             PERFORM H-UPPDATERA                                          
029600           END-IF                                                         
029700         ELSE                                                             
029800           IF MFS-FIRST                                                   
029900             PERFORM C-FOERSTA-SIDA                                       
030000           ELSE                                                           
030100             IF MFS-NEXT                                                  
030200               PERFORM D-NAESTA-SIDA                                      
030300             ELSE                                                         
030400               PERFORM E-SAMMA-SIDA                                       
030500             END-IF                                                       
030600           END-IF                                                         
030700         END-IF                                                           
030800         IF MFS-UPDATE AND INDATA-FEL                                     
030900            MOVE SPAR-KDARBTYP-ENTER TO W-KDARBTYP                        
031000            MOVE SPAR-IDPERSON-ENTER TO W-IDPERSON                        
031100            MOVE SPAR-IDARTNR-FOM-ENTER  TO W-IDARTNRF-MIN                
031200            MOVE SPAR-IDARTNR-TOM-ENTER  TO W-IDARTNRT-MIN                
031210            MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND21-MIN                
031300            MOVE SPAR-IDLEVNR-ENTER      TO W-IDLEVNR22-MIN               
031400                                            W-IDLEVNR-B-MIN               
031410            MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND22-MIN                
031420                                            W-IDLAND-B-MIN                
031500            MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRPF-MIN               
031600            MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRPT-MIN               
031610            MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND23-MIN                
031700         END-IF                                                           
031800         PERFORM F-LAS-VISA-INFO                                          
031900       END-IF                                                             
032000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O21701 + 4                      
032100       PERFORM IMS-INSERT-MSG                                             
032200     END-IF                                                               
032300                                                                          
032400     MOVE ZERO TO RETURN-CODE                                             
032500     GOBACK                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 A-INIT SECTION.                                                          
032900                                                                          
033000     IF MSG-DUBBLA-TRANSKODER                                             
033100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21701                 
033200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033400     ELSE                                                                 
033500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21701                  
033600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
033700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033800     END-IF                                                               
033900                                                                          
034000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
034100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
034200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034300                                                                          
034400     MOVE LOW-VALUE  TO MSG-AREA                                          
034500     MOVE 'W6O217N1' TO MFS-IDMOD                                         
034600     MOVE '6217'     TO MOD-IDTRANS                                       
034700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034800                                                                          
034900     IF EGEN-MID                                                          
035000       CONTINUE                                                           
035100     ELSE                                                                 
035200       MOVE SPACE TO MFS-KDTRTYP                                          
035300       MOVE '7' TO MFS-IDPFK                                              
035400     END-IF                                                               
035500                                                                          
035600     MOVE LOW-VALUE  TO W-WDP321KY-MIN-X                                  
035700                        W-WDP323KY-MIN-X                                  
035800                        W-WDP3A1-MIN                                      
035810                        W-WDP3B1-MIN                                      
035900                        W-WDP3C1-MIN                                      
036000     MOVE HIGH-VALUE TO W-WDP321KY-MAX-X                                  
036100                        W-WDP323KY-MAX-X                                  
036200                        W-WDP3A1-MAX                                      
036210                        W-WDP3B1-MAX                                      
036300                        W-WDP3C1-MAX                                      
036310                                                                          
036400     .                                                                    
036500     EJECT                                                                
036600 B-KOLLA-NYCKLAR SECTION.                                                 
036700                                                                          
036800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036900     MOVE '001'             TO MSGI-KDCALL                                
037000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037200     MOVE '6217'            TO MSGI-IDTRANS                               
037300                                                                          
037400     MOVE SPACE TO WS-KDARBVAL                                            
037500                   WS-IDPERSON-QUAL                                       
037600                   WS-IDPERSON-PACK                                       
037610                   WS-IDLANDX2                                            
037700                                                                          
037800     IF EGEN-MID                                                          
037900        MOVE MID-IDPERSON-QUAL-IN TO MSGI-IDPERSON-QUAL                   
038000        MOVE MID-IDPERSON-PACK-IN TO MSGI-IDPERSON-CDC                    
038100        MOVE MID-KDARBVAL-IN      TO MSGI-KDARBVAL                        
038110        MOVE MID-IDLANDX2-IN      TO MSGI-IDLANDX2                        
038200     END-IF                                                               
038300                                                                          
038400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038500     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
038600     MOVE MSGI-IDPERSON-QUAL TO WS-IDPERSON-QUAL                          
038700     MOVE MSGI-IDPERSON-CDC  TO WS-IDPERSON-PACK                          
038800     MOVE MSGI-KDARBVAL      TO WS-KDARBVAL                               
038810     MOVE MSGI-IDLANDX2      TO WS-IDLANDX2                               
038900                                                                          
039000     MOVE JA TO NYCKLAR-SW                                                
039100                                                                          
039200     INSPECT MSGI-IDPERSON-QUAL REPLACING LEADING SPACE BY ZERO           
039300     INSPECT MSGI-IDPERSON-CDC REPLACING LEADING SPACE BY ZERO            
039400     IF MSGI-IDPERSON-QUAL NUMERIC                                        
039500        CONTINUE                                                          
039600     ELSE                                                                 
039700        MOVE NEJ TO NYCKLAR-SW                                            
039800     END-IF                                                               
039810                                                                          
039900     IF MSGI-IDPERSON-CDC NUMERIC                                         
040000        CONTINUE                                                          
040100     ELSE                                                                 
040200        MOVE NEJ TO NYCKLAR-SW                                            
040300     END-IF                                                               
040400                                                                          
040403     IF MSGI-IDLANDX2 = WC-LAND-SE OR WC-LAND-CN OR WC-LAND-US            
040404       MOVE MSGI-IDLANDX2 TO W-IDLAND21-MIN                               
040405                             W-IDLAND21-MAX                               
040406                             W-IDLAND22-MIN                               
040407                             W-IDLAND22-MAX                               
040408                             W-IDLAND23-MIN                               
040409                             W-IDLAND23-MAX                               
040410                             W-IDLAND-A-MIN                               
040411                             W-IDLAND-A-MAX                               
040412                             W-IDLAND-B-MIN                               
040413                             W-IDLAND-B-MAX                               
040414                             W-IDLAND-C-MIN                               
040415                             W-IDLAND-C-MAX                               
040429     ELSE                                                                 
040430       MOVE NEJ TO NYCKLAR-SW                                             
040431     END-IF                                                               
040432                                                                          
040433     IF MSGI-IDPERSON-CDC NUMERIC                                         
040434        CONTINUE                                                          
040435     ELSE                                                                 
040440        MOVE NEJ TO NYCKLAR-SW                                            
040450     END-IF                                                               
040460                                                                          
040500     MOVE SPACE TO MSGI-KDARBTYP                                          
040600     IF MSGI-KDARBVAL NOT = SPACE                                         
040700        IF MSGI-KDARBVAL = 'Q' OR 'P'                                     
040800           MOVE MSGI-KDARBVAL TO WS-KDARBVAL                              
040900           IF MSGI-KDARBVAL = 'Q'                                         
041000              MOVE 'QUAL' TO W-KDARBTYP                                   
041100                             W-KDARBTYP-A-MIN                             
041200                             W-KDARBTYP-A-MAX                             
041300                             W-KDARBTYP-B-MIN                             
041310                             W-KDARBTYP-B-MAX                             
041400                             W-KDARBTYP-C-MIN                             
041500                             W-KDARBTYP-C-MAX                             
041600                             MSGI-KDARBTYP                                
041700           ELSE                                                           
041800              IF MSGI-KDARBVAL = 'P'                                      
041900                 MOVE 'CDC' TO W-KDARBTYP                                 
042000                               W-KDARBTYP-A-MIN                           
042100                               W-KDARBTYP-A-MAX                           
042200                               W-KDARBTYP-B-MIN                           
042210                               W-KDARBTYP-B-MAX                           
042300                               W-KDARBTYP-C-MIN                           
042400                               W-KDARBTYP-C-MAX                           
042500                               MSGI-KDARBTYP                              
042600              END-IF                                                      
042700           END-IF                                                         
042800        ELSE                                                              
042900           MOVE SPACE TO MSGI-KDARBTYP                                    
043000        END-IF                                                            
043100        IF MSGI-IDPERSON-QUAL = SPACE                                     
043200           CONTINUE                                                       
043300        ELSE                                                              
043400           IF MSGI-IDPERSON-QUAL NUMERIC                                  
043500           AND MSGI-IDPERSON-QUAL = ZERO                                  
043600              CONTINUE                                                    
043700           ELSE                                                           
043800              MOVE NEJ TO NYCKLAR-SW                                      
043900              MOVE SPACE TO MSGI-KDARBTYP                                 
044000           END-IF                                                         
044100        END-IF                                                            
044200        IF MSGI-IDPERSON-CDC = SPACE                                      
044300           CONTINUE                                                       
044400        ELSE                                                              
044500           IF MSGI-IDPERSON-CDC NUMERIC                                   
044600           AND MSGI-IDPERSON-CDC = ZERO                                   
044700              CONTINUE                                                    
044800           ELSE                                                           
044900              MOVE SPACE TO MSGI-KDARBTYP                                 
045000              MOVE NEJ TO NYCKLAR-SW                                      
045100           END-IF                                                         
045200        END-IF                                                            
045300     ELSE                                                                 
045400        IF MSGI-IDPERSON-QUAL NOT = ZERO                                  
045500           IF MSGI-IDPERSON-CDC NOT = ZERO                                
045600              MOVE NEJ TO NYCKLAR-SW                                      
045700           END-IF                                                         
045800        END-IF                                                            
045900        IF NYCKLAR-OK                                                     
046000           IF MSGI-IDPERSON-QUAL NUMERIC                                  
046100              IF MSGI-IDPERSON-QUAL > ZERO                                
046200                 MOVE 'QUAL' TO MSGI-KDARBTYP                             
046300                                W-KDARBTYP                                
046400                                W-KDARBTYP-A-MIN                          
046500                                W-KDARBTYP-A-MAX                          
046600                                W-KDARBTYP-B-MIN                          
046610                                W-KDARBTYP-B-MAX                          
046700                                W-KDARBTYP-C-MIN                          
046800                                W-KDARBTYP-C-MAX                          
046900                 MOVE MSGI-IDPERSON-QUAL TO W-IDPERSON                    
047000              END-IF                                                      
047100           END-IF                                                         
047200           IF MSGI-IDPERSON-CDC NUMERIC                                   
047300              IF MSGI-IDPERSON-CDC > ZERO                                 
047400                 MOVE 'CDC' TO MSGI-KDARBTYP                              
047500                               W-KDARBTYP                                 
047600                               W-KDARBTYP-A-MIN                           
047700                               W-KDARBTYP-A-MAX                           
047810                               W-KDARBTYP-B-MIN                           
047820                               W-KDARBTYP-B-MAX                           
047900                               W-KDARBTYP-C-MIN                           
048000                               W-KDARBTYP-C-MAX                           
048100                 MOVE MSGI-IDPERSON-CDC TO W-IDPERSON                     
048200              END-IF                                                      
048300           END-IF                                                         
048400        END-IF                                                            
048500     END-IF                                                               
048600                                                                          
048700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048800     EJECT                                                                
048900                                                                          
049000     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL-IN                         
049100     IF MID-IDPERSON-QUAL-IN NOT = ALL '+'                                
049200        MOVE '7'   TO MFS-IDPFK                                           
049300        MOVE SPACE TO MFS-KDTRTYP                                         
049400        PERFORM MFS-RENSA-SPAR                                            
049500     END-IF                                                               
049600                                                                          
049700     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-PACK-IN                         
049800     IF MID-IDPERSON-PACK-IN NOT = ALL '+'                                
049900        MOVE '7'   TO MFS-IDPFK                                           
050000        PERFORM MFS-RENSA-SPAR                                            
050100        MOVE SPACE TO MFS-KDTRTYP                                         
050200     END-IF                                                               
050300                                                                          
050400     MOVE MFS-RENSA-FAELT TO MOD-KDARBVAL-IN                              
050500     IF MID-KDARBVAL-IN NOT = ALL '+'                                     
050600        MOVE '7'   TO MFS-IDPFK                                           
050700        MOVE SPACE TO MFS-KDTRTYP                                         
050800        PERFORM MFS-RENSA-SPAR                                            
050900     END-IF                                                               
051000                                                                          
051010     MOVE MFS-RENSA-FAELT TO MOD-IDLANDX2-IN                              
051020     IF MID-IDLANDX2-IN NOT = ALL '+'                                     
051030        MOVE '7'   TO MFS-IDPFK                                           
051040        MOVE SPACE TO MFS-KDTRTYP                                         
051050        PERFORM MFS-RENSA-SPAR                                            
051060     END-IF                                                               
051070                                                                          
051100     IF GODK-MID OR NYCKLAR-OK                                            
051200        MOVE MSGI-IDPERSON-QUAL TO MOD-IDPERSON-QUAL-UT                   
051300        INSPECT MOD-IDPERSON-QUAL-UT                                      
051400                REPLACING LEADING ZERO BY SPACE                           
051500        MOVE MSGI-IDPERSON-CDC TO MOD-IDPERSON-PACK-UT                    
051600        INSPECT MOD-IDPERSON-PACK-UT                                      
051700                REPLACING LEADING ZERO BY SPACE                           
051800        MOVE MSGI-KDARBVAL TO MOD-KDARBVAL-UT                             
051810        MOVE MSGI-IDLANDX2 TO MOD-IDLANDX2-UT                             
051900     ELSE                                                                 
052000        MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL-UT                      
052100                                MOD-IDPERSON-PACK-UT                      
052200                                MOD-KDARBVAL-UT                           
052210                                MOD-IDLANDX2-UT                           
052300     END-IF                                                               
052400                                                                          
052500     IF NYCKLAR-FEL                                                       
052600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
052700        CALL WMEDKONV USING MED-WMEDAREA                                  
052800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
052900        PERFORM MFS-RENSA-FAELT-IN                                        
053000        PERFORM MFS-RENSA-FAELT-UT                                        
053100        PERFORM MFS-RENSA-SPAR                                            
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 C-FOERSTA-SIDA SECTION.                                                  
053600                                                                          
053700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
053800     CALL WMEDKONV USING MED-WMEDAREA                                     
053900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
054000     PERFORM MFS-RENSA-FAELT-IN                                           
054100     .                                                                    
054200     EJECT                                                                
054300 D-NAESTA-SIDA SECTION.                                                   
054400                                                                          
054500     MOVE NEJ TO WS-SISTA-SIDAN                                           
054600                                                                          
054700     IF SPAR-IDTRANS = '6217'                                             
054800       MOVE SPAR-KDARBTYP-NEXT     TO W-KDARBTYP                          
054900                                      W-KDARBTYP-A-MIN                    
055000                                      W-KDARBTYP-A-MAX                    
055100                                      W-KDARBTYP-B-MIN                    
055110                                      W-KDARBTYP-B-MAX                    
055200                                      W-KDARBTYP-C-MIN                    
055300                                      W-KDARBTYP-C-MAX                    
055310       IF SPAR-IDPERSON-NEXT NUMERIC                                      
055400        MOVE SPAR-IDPERSON-NEXT     TO W-IDPERSON                         
055410       ELSE                                                               
055420        MOVE ZERO TO W-IDPERSON                                           
055430       END-IF                                                             
055500       MOVE SPAR-IDARTNR-FOM-NEXT  TO W-IDARTNRF-MIN                      
055600                                      W-IDARTNRF-A-MIN                    
055700       MOVE SPAR-IDARTNR-TOM-NEXT  TO W-IDARTNRT-MIN                      
055800                                      W-IDARTNRT-A-MIN                    
055810       MOVE SPAR-IDLANDX2-NEXT     TO W-IDLAND21-MIN                      
055820                                      W-IDLAND-A-MIN                      
055900       MOVE SPAR-IDLEVNR-NEXT      TO W-IDLEVNR22-MIN                     
056000                                      W-IDLEVNR-B-MIN                     
056010       MOVE SPAR-IDLANDX2-NEXT     TO W-IDLAND22-MIN                      
056020                                      W-IDLAND-B-MIN                      
056100       MOVE SPAR-IDFKNGRP-FOM-NEXT TO W-IDFKNGRPF-MIN                     
056200                                      W-IDFKNGRPF-C-MIN                   
056300       MOVE SPAR-IDFKNGRP-TOM-NEXT TO W-IDFKNGRPT-MIN                     
056400                                      W-IDFKNGRPT-C-MIN                   
056410       MOVE SPAR-IDLANDX2-NEXT     TO W-IDLAND23-MIN                      
056420                                      W-IDLAND-C-MIN                      
056500                                                                          
056600       IF SPAR-IDARTNR-FOM-NEXT = 999999999                               
056700       AND SPAR-IDLEVNR-NEXT = HIGH-VALUE                                 
056800       AND SPAR-IDFKNGRP-FOM-NEXT = 9999                                  
056810*      AND SPAR-IDLANDX2-NEXT     = HIGH-VALUE                            
056900          MOVE SPAR-KDARBTYP-ENTER TO W-KDARBTYP                          
057000                                      W-KDARBTYP-A-MIN                    
057100                                      W-KDARBTYP-A-MAX                    
057200                                      W-KDARBTYP-B-MIN                    
057210                                      W-KDARBTYP-B-MAX                    
057300                                      W-KDARBTYP-C-MIN                    
057400                                      W-KDARBTYP-C-MAX                    
057500          MOVE SPAR-IDPERSON-ENTER TO W-IDPERSON                          
057600          MOVE SPAR-IDARTNR-FOM-ENTER  TO W-IDARTNRF-MIN                  
057700                                          W-IDARTNRF-A-MIN                
057800          MOVE SPAR-IDARTNR-TOM-ENTER  TO W-IDARTNRT-MIN                  
057900                                          W-IDARTNRT-A-MIN                
057910          MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND21-MIN                  
057920                                          W-IDLAND-A-MIN                  
058000          MOVE SPAR-IDLEVNR-ENTER      TO W-IDLEVNR22-MIN                 
058100                                          W-IDLEVNR-B-MIN                 
058101          MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND22-MIN                  
058102                                          W-IDLAND-B-MIN                  
058110                                                                          
058200          MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRPF-MIN                 
058300                                          W-IDFKNGRPF-C-MIN               
058400          MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRPT-MIN                 
058500                                          W-IDFKNGRPT-C-MIN               
058510          MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND23-MIN                  
058520                                          W-IDLAND-C-MIN                  
058600          MOVE INF-LAST-PAGE TO MED-IDMFSFEL                              
058700          CALL WMEDKONV USING MED-WMEDAREA                                
058800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
058900          MOVE JA TO WS-SISTA-SIDAN                                       
059000       END-IF                                                             
059100       PERFORM MFS-RENSA-FAELT-IN                                         
059200     ELSE                                                                 
059300       PERFORM MFS-RENSA-FAELT-IN                                         
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 E-SAMMA-SIDA SECTION.                                                    
059800                                                                          
059900     IF SPAR-IDTRANS = '6217'                                             
060000        MOVE SPAR-KDARBTYP-ENTER TO W-KDARBTYP                            
060100                                    W-KDARBTYP-A-MIN                      
060200                                    W-KDARBTYP-A-MAX                      
060300                                    W-KDARBTYP-B-MIN                      
060310                                    W-KDARBTYP-B-MAX                      
060400                                    W-KDARBTYP-C-MIN                      
060500                                    W-KDARBTYP-C-MAX                      
060600        MOVE SPAR-IDPERSON-ENTER TO W-IDPERSON                            
060700        MOVE SPAR-IDARTNR-FOM-ENTER  TO W-IDARTNRF-MIN                    
060800                                        W-IDARTNRF-A-MIN                  
060900        MOVE SPAR-IDARTNR-TOM-ENTER  TO W-IDARTNRT-MIN                    
061000                                        W-IDARTNRT-A-MIN                  
061010        MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND21-MIN                    
061011                                        W-IDLAND-A-MIN                    
061020*                                                                         
061100        MOVE SPAR-IDLEVNR-ENTER      TO W-IDLEVNR22-MIN                   
061200                                        W-IDLEVNR-B-MIN                   
061220        MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND22-MIN                    
061221                                        W-IDLAND-B-MIN                    
061230*                                                                         
061300        MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRPF-MIN                   
061400                                        W-IDFKNGRPF-C-MIN                 
061500        MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRPT-MIN                   
061600                                        W-IDFKNGRPT-C-MIN                 
061610        MOVE SPAR-IDLANDX2-ENTER     TO W-IDLAND23-MIN                    
061620                                        W-IDLAND-C-MIN                    
061700                                                                          
061800        MOVE NEJ TO INPUT-SW                                              
061900        PERFORM S01-KOLLA-INPUT                                           
062000        IF INPUT-SAKNAS                                                   
062100           PERFORM MFS-RENSA-FAELT-IN                                     
062200        ELSE                                                              
062300           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
062400           CALL WMEDKONV USING MED-WMEDAREA                               
062500           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
062600           PERFORM EA-MID-INDATA-TILL-MOD                                 
062700        END-IF                                                            
062800     ELSE                                                                 
062900        PERFORM MFS-RENSA-FAELT-IN                                        
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300 EA-MID-INDATA-TILL-MOD SECTION.                                          
063400                                                                          
063500     MOVE +1 TO IX                                                        
063600     PERFORM UNTIL IX > IX-MAX                                            
063700        IF MID-KDCMDVAL(IX) NOT = ALL '+'                                 
063800           MOVE MID-KDCMDVAL(IX) TO MOD-KDCMDVAL(IX)                      
063900           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR(IX)            
064000        ELSE                                                              
064100           MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(IX)                       
064200        END-IF                                                            
064300        ADD +1 TO IX                                                      
064400     END-PERFORM                                                          
064500                                                                          
064600     IF MID-IDPERSON-QUAL-NY NOT = ALL '+'                                
064700        MOVE MID-IDPERSON-QUAL-NY TO MOD-IDPERSON-QUAL-NY                 
064800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-QUAL-ATTR              
064900     ELSE                                                                 
065000        MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL-NY                      
065100     END-IF                                                               
065200                                                                          
065300     IF MID-IDPERSON-PACK-NY NOT = ALL '+'                                
065400        MOVE MID-IDPERSON-PACK-NY TO MOD-IDPERSON-PACK-NY                 
065500        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-PACK-ATTR              
065600     ELSE                                                                 
065700        MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-PACK-NY                      
065800     END-IF                                                               
065900                                                                          
066000     IF MID-IDARTNR-FOM-NY NOT = ALL '+'                                  
066100        MOVE MID-IDARTNR-FOM-NY TO MOD-IDARTNR-FOM-NY                     
066200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-FOM-ATTR                
066300     ELSE                                                                 
066400        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-FOM-NY                        
066500     END-IF                                                               
066600                                                                          
066700     IF MID-IDARTNR-TOM-NY NOT = ALL '+'                                  
066800        MOVE MID-IDARTNR-TOM-NY TO MOD-IDARTNR-TOM-NY                     
066900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-TOM-ATTR                
067000     ELSE                                                                 
067100        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-TOM-NY                        
067200     END-IF                                                               
067300                                                                          
067400     IF MID-IDLEVNR-NY NOT = ALL '+'                                      
067500        MOVE MID-IDLEVNR-NY        TO MOD-IDLEVNR-NY                      
067600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR                    
067700     ELSE                                                                 
067800        MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-NY                      
067900     END-IF                                                               
068000                                                                          
068100     IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                                 
068200        MOVE MID-IDFKNGRP-FOM-NY   TO MOD-IDFKNGRP-FOM-NY                 
068300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-FOM-ATTR               
068400     ELSE                                                                 
068500        MOVE MFS-RENSA-FAELT       TO MOD-IDFKNGRP-FOM-NY                 
068600     END-IF                                                               
068700                                                                          
068800     IF MID-IDFKNGRP-TOM-NY NOT = ALL '+'                                 
068900        MOVE MID-IDFKNGRP-TOM-NY   TO MOD-IDFKNGRP-TOM-NY                 
069000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-TOM-ATTR               
069100     ELSE                                                                 
069200        MOVE MFS-RENSA-FAELT       TO MOD-IDFKNGRP-TOM-NY                 
069300     END-IF                                                               
069310                                                                          
069320     IF MID-IDLANDX2-NY NOT = ALL '+'                                     
069330        MOVE MID-IDLANDX2-NY       TO MOD-IDLANDX2-NY                     
069340        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLANDX2-ATTR                   
069350     ELSE                                                                 
069360        MOVE MFS-RENSA-FAELT       TO MOD-IDLANDX2-NY                     
069370     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 F-LAS-VISA-INFO SECTION.                                                 
069700                                                                          
069800     MOVE SPACE TO INTERVALL-SW                                           
069900     IF WS-KDARBVAL = 'Q' OR 'P'                                          
070000        PERFORM FC-LAS-KDARBTYP                                           
070100     ELSE                                                                 
070200        PERFORM FB-LAS-UNIK-IDPERSON                                      
070300     END-IF                                                               
070400                                                                          
070500     IF SEGMENT-FINNS                                                     
070600        IF WS-KDARBVAL = 'Q' OR 'P'                                       
070700           IF INTERVALL-ARTNR                                             
070800              MOVE SEQA-KDARBTYP TO SPAR-KDARBTYP-NEXT                    
070900              MOVE SEQA-IDPERSON TO SPAR-IDPERSON-NEXT                    
071000              MOVE SEQA-IDARTNR-FOM  TO SPAR-IDARTNR-FOM-NEXT             
071100              MOVE SEQA-IDARTNR-TOM  TO SPAR-IDARTNR-TOM-NEXT             
071110              MOVE SEQA-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
071200              MOVE SPACE             TO SPAR-IDLEVNR-NEXT                 
071300              MOVE ZERO              TO SPAR-IDFKNGRP-FOM-NEXT            
071400              MOVE 9999              TO SPAR-IDFKNGRP-TOM-NEXT            
071500           END-IF                                                         
071600           IF INTERVALL-LEVNR                                             
071700              MOVE SEQB-KDARBTYP TO SPAR-KDARBTYP-NEXT                    
071800              MOVE SEQB-IDPERSON TO SPAR-IDPERSON-NEXT                    
071900              MOVE SEQB-IDLEVNR      TO SPAR-IDLEVNR-NEXT                 
071910              MOVE SEQB-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
072000              MOVE ZERO  TO SPAR-IDFKNGRP-FOM-NEXT                        
072100              MOVE 9999  TO SPAR-IDFKNGRP-TOM-NEXT                        
072200              MOVE 999999999 TO SPAR-IDARTNR-FOM-NEXT                     
072300                                SPAR-IDARTNR-TOM-NEXT                     
072400           END-IF                                                         
072500           IF INTERVALL-FKNGRP                                            
072600              MOVE SEQC-KDARBTYP TO SPAR-KDARBTYP-NEXT                    
072700              MOVE SEQC-IDPERSON TO SPAR-IDPERSON-NEXT                    
072800              MOVE SEQC-IDFKNGRP-FOM TO SPAR-IDFKNGRP-FOM-NEXT            
072900              MOVE SEQC-IDFKNGRP-TOM TO SPAR-IDFKNGRP-TOM-NEXT            
072910              MOVE SEQC-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
073000              MOVE 999999999     TO SPAR-IDARTNR-FOM-NEXT                 
073100                                    SPAR-IDARTNR-TOM-NEXT                 
073200              MOVE HIGH-VALUE    TO SPAR-IDLEVNR-NEXT                     
073300           END-IF                                                         
073400           MOVE ARBT-KDARBTYP TO SPAR-KDARBTYP-NEXT                       
073500           MOVE PERS-IDPERSON TO SPAR-IDPERSON-NEXT                       
073600        ELSE                                                              
073700           IF INTERVALL-ARTNR                                             
073800              MOVE IART-IDARTNR-FOM  TO SPAR-IDARTNR-FOM-NEXT             
073900              MOVE IART-IDARTNR-TOM  TO SPAR-IDARTNR-TOM-NEXT             
073910              MOVE IART-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
074000              MOVE SPACE             TO SPAR-IDLEVNR-NEXT                 
074100              MOVE ZERO              TO SPAR-IDFKNGRP-FOM-NEXT            
074200              MOVE 9999              TO SPAR-IDFKNGRP-TOM-NEXT            
074300           END-IF                                                         
074400           IF INTERVALL-LEVNR                                             
074500              MOVE ILEV-IDLEVNR      TO SPAR-IDLEVNR-NEXT                 
074510              MOVE ILEV-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
074600              MOVE ZERO  TO SPAR-IDFKNGRP-FOM-NEXT                        
074700              MOVE 9999  TO SPAR-IDFKNGRP-TOM-NEXT                        
074800              MOVE 999999999 TO SPAR-IDARTNR-FOM-NEXT                     
074900                                SPAR-IDARTNR-TOM-NEXT                     
075000           END-IF                                                         
075100           IF INTERVALL-FKNGRP                                            
075200              MOVE IFKN-IDFKNGRP-FOM TO SPAR-IDFKNGRP-FOM-NEXT            
075300              MOVE IFKN-IDFKNGRP-TOM TO SPAR-IDFKNGRP-TOM-NEXT            
075310              MOVE IFKN-IDLANDX2     TO SPAR-IDLANDX2-NEXT                
075400              MOVE 999999999         TO SPAR-IDARTNR-FOM-NEXT             
075500                                        SPAR-IDARTNR-TOM-NEXT             
075600              MOVE HIGH-VALUE        TO SPAR-IDLEVNR-NEXT                 
075700           END-IF                                                         
075800           MOVE ARBT-KDARBTYP TO SPAR-KDARBTYP-NEXT                       
075900           MOVE PERS-IDPERSON TO SPAR-IDPERSON-NEXT                       
076000        END-IF                                                            
076100        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
076200        CALL WMEDKONV USING MED-WMEDAREA                                  
076300        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
076400     ELSE                                                                 
076500        MOVE MFS-RENSA-FAELT TO SPAR-KDARBTYP-NEXT                        
076600                                SPAR-IDPERSON-NEXT                        
076700        MOVE 999999999       TO SPAR-IDARTNR-FOM-NEXT                     
076800                                SPAR-IDARTNR-TOM-NEXT                     
076900        MOVE HIGH-VALUE      TO SPAR-IDLEVNR-NEXT                         
077000        MOVE 9999            TO SPAR-IDFKNGRP-FOM-NEXT                    
077100                                SPAR-IDFKNGRP-TOM-NEXT                    
077110        MOVE HIGH-VALUE      TO SPAR-IDLANDX2-NEXT                        
077200     END-IF                                                               
077300                                                                          
077400     MOVE '002'  TO MSGI-KDCALL                                           
077500     MOVE '6217' TO SPAR-IDTRANS                                          
077600     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
077700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
077800     .                                                                    
077900     EJECT                                                                
078000 FB-LAS-UNIK-IDPERSON SECTION.                                            
078100                                                                          
078200     PERFORM IMS-GET-WDP301                                               
078300     PERFORM IMS-GET-WDP311                                               
078400     IF SEGMENT-SAKNAS                                                    
078500        IF WS-SISTA-SIDAN = JA                                            
078600           CONTINUE                                                       
078700        ELSE                                                              
078800           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
078900           CALL WMEDKONV USING MED-WMEDAREA                               
079000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
079100        END-IF                                                            
079200        PERFORM MFS-RENSA-FAELT-UT                                        
079300     ELSE                                                                 
079400        MOVE +1 TO IX                                                     
079500        PERFORM FBA-LAS-RADDATA                                           
079600        IF SEGMENT-SAKNAS                                                 
079700           IF WS-SISTA-SIDAN = JA                                         
079800              CONTINUE                                                    
079900           ELSE                                                           
080000              MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                       
080100              CALL WMEDKONV USING MED-WMEDAREA                            
080200              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
080300           END-IF                                                         
080400           PERFORM MFS-RENSA-FAELT-UT                                     
080500        ELSE                                                              
080600           MOVE ARBT-KDARBTYP TO SPAR-KDARBTYP-ENTER                      
080700           MOVE PERS-IDPERSON TO SPAR-IDPERSON-ENTER                      
080800           IF INTERVALL-ARTNR                                             
080900              MOVE IART-IDARTNR-FOM TO SPAR-IDARTNR-FOM-ENTER             
081000              MOVE IART-IDARTNR-TOM TO SPAR-IDARTNR-TOM-ENTER             
081010              MOVE IART-IDLANDX2    TO SPAR-IDLANDX2-ENTER                
081100              MOVE SPACE            TO SPAR-IDLEVNR-ENTER                 
081200              MOVE ZERO             TO SPAR-IDFKNGRP-FOM-ENTER            
081300              MOVE 9999             TO SPAR-IDFKNGRP-TOM-ENTER            
081400           ELSE                                                           
081500              IF INTERVALL-LEVNR                                          
081600                 MOVE ILEV-IDLEVNR     TO SPAR-IDLEVNR-ENTER              
081620                 MOVE ILEV-IDLANDX2    TO SPAR-IDLANDX2-ENTER             
081700                 MOVE 999999999        TO SPAR-IDARTNR-FOM-ENTER          
081800                                          SPAR-IDARTNR-TOM-ENTER          
081900                 MOVE ZERO             TO SPAR-IDFKNGRP-FOM-ENTER         
082000                 MOVE 9999             TO SPAR-IDFKNGRP-TOM-ENTER         
082100              ELSE                                                        
082200                 IF INTERVALL-FKNGRP                                      
082300                    MOVE IFKN-IDFKNGRP-FOM                                
082400                                    TO SPAR-IDFKNGRP-FOM-ENTER            
082500                    MOVE IFKN-IDFKNGRP-TOM                                
082600                                    TO SPAR-IDFKNGRP-TOM-ENTER            
082610                    MOVE IFKN-IDLANDX2                                    
082620                                    TO SPAR-IDLANDX2-ENTER                
082700                    MOVE 999999999  TO SPAR-IDARTNR-FOM-ENTER             
082800                                       SPAR-IDARTNR-TOM-ENTER             
082900                    MOVE HIGH-VALUE TO SPAR-IDLEVNR-ENTER                 
083000                 END-IF                                                   
083100              END-IF                                                      
083200           END-IF                                                         
083300                                                                          
083400           PERFORM UNTIL IX > IX-MAX                                      
083500              IF SEGMENT-FINNS                                            
083600                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-FOM(IX)              
083700                                         MOD-IDARTNR-TOM(IX)              
083800                                         MOD-IDLEVNR(IX)                  
083900                                         MOD-IDFKNGRP-TOM(IX)             
084000                                         MOD-IDFKNGRP-FOM(IX)             
084100                 MOVE MFS-STAENG-FAELT                                    
084200                                   TO MOD-KDCMDVAL-ATTR(IX)               
084300                 IF ARBT-KDARBTYP = 'QUAL'                                
084400                    MOVE PERS-IDPERSON TO MOD-IDPERSON-QUAL(IX)           
084500                    MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-PACK(IX)         
084600                 ELSE                                                     
084700                    MOVE PERS-IDPERSON TO MOD-IDPERSON-PACK(IX)           
084800                    MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL(IX)         
084900                 END-IF                                                   
085000                 IF INTERVALL-ARTNR                                       
085100                    MOVE IART-IDARTNR-FOM TO MOD-IDARTNR-FOM(IX)          
085200                    MOVE IART-IDARTNR-TOM TO MOD-IDARTNR-TOM(IX)          
085300                    MOVE MFS-OEPPNA-ALFA-FAELT-HI                         
085400                                   TO MOD-KDCMDVAL-ATTR(IX)               
085500                 ELSE                                                     
085600                    IF INTERVALL-LEVNR                                    
085700                       MOVE ILEV-IDLEVNR                                  
085800                                     TO MOD-IDLEVNR(IX)                   
085900                       MOVE MFS-OEPPNA-ALFA-FAELT-HI                      
086000                                     TO MOD-KDCMDVAL-ATTR(IX)             
086100                    ELSE                                                  
086200                       IF INTERVALL-FKNGRP                                
086300                          MOVE IFKN-IDFKNGRP-FOM                          
086400                                         TO MOD-IDFKNGRP-FOM(IX)          
086500                          MOVE IFKN-IDFKNGRP-TOM                          
086600                                         TO MOD-IDFKNGRP-TOM(IX)          
086700                          MOVE MFS-OEPPNA-ALFA-FAELT-HI                   
086800                                     TO MOD-KDCMDVAL-ATTR(IX)             
086900                       END-IF                                             
087000                    END-IF                                                
087100                 END-IF                                                   
087200                 PERFORM FBA-LAS-RADDATA                                  
087300              ELSE                                                        
087400                 MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL(IX)            
087500                                         MOD-IDPERSON-PACK(IX)            
087600                                         MOD-IDARTNR-FOM(IX)              
087700                                         MOD-IDARTNR-TOM(IX)              
087800                                         MOD-IDLEVNR(IX)                  
087900                                         MOD-IDFKNGRP-FOM(IX)             
088000                                         MOD-IDFKNGRP-TOM(IX)             
088100              END-IF                                                      
088200              ADD 1 TO IX                                                 
088300           END-PERFORM                                                    
088400        END-IF                                                            
088500     END-IF                                                               
088600     .                                                                    
088700     EJECT                                                                
088800 FBA-LAS-RADDATA SECTION.                                                 
088900                                                                          
089000     PERFORM IMS-GET-WDP321                                               
089100     IF SEGMENT-FINNS                                                     
089200        MOVE 'A' TO INTERVALL-SW                                          
089300     ELSE                                                                 
089400        PERFORM IMS-GET-WDP322                                            
089500        IF SEGMENT-FINNS                                                  
089600           MOVE 'L' TO INTERVALL-SW                                       
089700        ELSE                                                              
089800           PERFORM IMS-GET-WDP323                                         
089900           IF SEGMENT-FINNS                                               
090000              MOVE 'F' TO INTERVALL-SW                                    
090100           END-IF                                                         
090200        END-IF                                                            
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600 FC-LAS-KDARBTYP SECTION.                                                 
090700                                                                          
090800     PERFORM IMS-GET-WDP301                                               
090900     IF SEGMENT-SAKNAS                                                    
091000        IF WS-SISTA-SIDAN = JA                                            
091100           CONTINUE                                                       
091200        ELSE                                                              
091300           MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                          
091400           CALL WMEDKONV USING MED-WMEDAREA                               
091500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
091600        END-IF                                                            
091700        PERFORM MFS-RENSA-FAELT-UT                                        
091800     ELSE                                                                 
091900        PERFORM FCA-SOEK-FORSTA                                           
092000        IF SEGMENT-SAKNAS                                                 
092100           IF WS-SISTA-SIDAN = JA                                         
092200              CONTINUE                                                    
092300           ELSE                                                           
092400              MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                       
092500              CALL WMEDKONV USING MED-WMEDAREA                            
092600              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
092700           END-IF                                                         
092800           PERFORM MFS-RENSA-FAELT-UT                                     
092900        ELSE                                                              
093000           MOVE +1 TO IX                                                  
093100           MOVE ARBT-KDARBTYP TO SPAR-KDARBTYP-ENTER                      
093200           IF INTERVALL-ARTNR                                             
093300              MOVE SEQA-IDPERSON TO SPAR-IDPERSON-ENTER                   
093400              MOVE SEQA-IDARTNR-FOM TO SPAR-IDARTNR-FOM-ENTER             
093500              MOVE SEQA-IDARTNR-TOM TO SPAR-IDARTNR-TOM-ENTER             
093510              MOVE SEQA-IDLANDX2     TO SPAR-IDLANDX2-ENTER               
093600              MOVE SPACE            TO SPAR-IDLEVNR-ENTER                 
093700              MOVE ZERO             TO SPAR-IDFKNGRP-FOM-ENTER            
093800              MOVE 9999             TO SPAR-IDFKNGRP-TOM-ENTER            
093900           ELSE                                                           
094000             IF INTERVALL-LEVNR                                           
094100               MOVE SEQB-IDPERSON    TO SPAR-IDPERSON-ENTER               
094200               MOVE SEQB-IDLEVNR     TO SPAR-IDLEVNR-ENTER                
094210               MOVE SEQB-IDLANDX2    TO SPAR-IDLANDX2-ENTER               
094300               MOVE 999999999        TO SPAR-IDARTNR-FOM-ENTER            
094400                                        SPAR-IDARTNR-TOM-ENTER            
094500               MOVE ZERO             TO SPAR-IDFKNGRP-FOM-ENTER           
094600               MOVE 9999             TO SPAR-IDFKNGRP-TOM-ENTER           
094700             ELSE                                                         
094800               IF INTERVALL-FKNGRP                                        
094900                 MOVE SEQC-IDPERSON  TO SPAR-IDPERSON-ENTER               
095000                 MOVE SEQC-IDFKNGRP-FOM                                   
095100                                  TO SPAR-IDFKNGRP-FOM-ENTER              
095200                 MOVE SEQC-IDFKNGRP-TOM                                   
095300                                  TO SPAR-IDFKNGRP-TOM-ENTER              
095310                 MOVE SEQC-IDLANDX2                                       
095320                                  TO SPAR-IDLANDX2-ENTER                  
095400                 MOVE 999999999   TO SPAR-IDARTNR-FOM-ENTER               
095500                                     SPAR-IDARTNR-TOM-ENTER               
095600                 MOVE HIGH-VALUE  TO SPAR-IDLEVNR-ENTER                   
095700               END-IF                                                     
095800             END-IF                                                       
095900           END-IF                                                         
096000                                                                          
096100           PERFORM UNTIL IX > IX-MAX                                      
096200              IF SEGMENT-FINNS                                            
096300                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-FOM(IX)              
096400                                         MOD-IDARTNR-TOM(IX)              
096500                                         MOD-IDLEVNR(IX)                  
096600                                         MOD-IDFKNGRP-FOM(IX)             
096700                                         MOD-IDFKNGRP-TOM(IX)             
096800                 MOVE MFS-STAENG-FAELT                                    
096900                                   TO MOD-KDCMDVAL-ATTR(IX)               
097000                 IF INTERVALL-ARTNR                                       
097100                   IF ARBT-KDARBTYP = 'QUAL'                              
097200                      MOVE SEQA-IDPERSON                                  
097300                                   TO MOD-IDPERSON-QUAL(IX)               
097400                      MOVE MFS-RENSA-FAELT                                
097500                                   TO MOD-IDPERSON-PACK(IX)               
097600                   ELSE                                                   
097700                      MOVE SEQA-IDPERSON                                  
097800                                   TO MOD-IDPERSON-PACK(IX)               
097900                      MOVE MFS-RENSA-FAELT                                
098000                                   TO MOD-IDPERSON-QUAL(IX)               
098100                   END-IF                                                 
098200                   MOVE SEQA-IDARTNR-FOM TO MOD-IDARTNR-FOM(IX)           
098300                   MOVE SEQA-IDARTNR-TOM TO MOD-IDARTNR-TOM(IX)           
098400                   MOVE MFS-OEPPNA-ALFA-FAELT-HI                          
098500                                 TO MOD-KDCMDVAL-ATTR(IX)                 
098600                 ELSE                                                     
098700                    IF INTERVALL-LEVNR                                    
098800                       IF ARBT-KDARBTYP = 'QUAL'                          
098900                          MOVE SEQB-IDPERSON                              
099000                                       TO MOD-IDPERSON-QUAL(IX)           
099100                          MOVE MFS-RENSA-FAELT                            
099200                                      TO MOD-IDPERSON-PACK(IX)            
099300                       ELSE                                               
099400                          MOVE SEQB-IDPERSON                              
099500                                      TO MOD-IDPERSON-PACK(IX)            
099600                         MOVE MFS-RENSA-FAELT                             
099700                                      TO MOD-IDPERSON-QUAL(IX)            
099800                       END-IF                                             
099900                       MOVE SEQB-IDLEVNR                                  
100000                                     TO MOD-IDLEVNR(IX)                   
100100                       MOVE MFS-OEPPNA-ALFA-FAELT-HI                      
100200                                     TO MOD-KDCMDVAL-ATTR(IX)             
100300                    ELSE                                                  
100400                       IF INTERVALL-FKNGRP                                
100500                          IF ARBT-KDARBTYP = 'QUAL'                       
100600                             MOVE SEQC-IDPERSON                           
100700                                       TO MOD-IDPERSON-QUAL(IX)           
100800                             MOVE MFS-RENSA-FAELT                         
100900                                       TO MOD-IDPERSON-PACK(IX)           
101000                          ELSE                                            
101100                             MOVE SEQC-IDPERSON                           
101200                                       TO MOD-IDPERSON-PACK(IX)           
101300                            MOVE MFS-RENSA-FAELT                          
101400                                       TO MOD-IDPERSON-QUAL(IX)           
101500                          END-IF                                          
101600                          MOVE SEQC-IDFKNGRP-FOM                          
101700                                     TO MOD-IDFKNGRP-FOM(IX)              
101800                          MOVE SEQC-IDFKNGRP-TOM                          
101900                                     TO MOD-IDFKNGRP-TOM(IX)              
102000                          MOVE MFS-OEPPNA-ALFA-FAELT-HI                   
102100                                     TO MOD-KDCMDVAL-ATTR(IX)             
102200                       END-IF                                             
102300                    END-IF                                                
102400                 END-IF                                                   
102500                 PERFORM FCB-SOEK-NASTA                                   
102600              ELSE                                                        
102700                 MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL(IX)            
102800                                         MOD-IDPERSON-PACK(IX)            
102900                                         MOD-IDARTNR-FOM(IX)              
103000                                         MOD-IDARTNR-TOM(IX)              
103100                                         MOD-IDLEVNR(IX)                  
103200                                         MOD-IDFKNGRP-FOM(IX)             
103300                                         MOD-IDFKNGRP-TOM(IX)             
103400              END-IF                                                      
103500              ADD 1 TO IX                                                 
103600           END-PERFORM                                                    
103700        END-IF                                                            
103800     END-IF                                                               
103900     .                                                                    
104000     EJECT                                                                
104100 FCA-SOEK-FORSTA SECTION.                                                 
104200                                                                          
104300     MOVE NEJ TO ART-SW                                                   
104400                 LEV-SW                                                   
104500                 FKN-SW                                                   
104600                 TRAEFF-SW                                                
104700                                                                          
104800     PERFORM IMS-GU-WDP3A                                                 
104900     PERFORM UNTIL SEGMENT-SAKNAS OR ART-SW = JA                          
105000        IF SEGMENT-FINNS                                                  
105100           MOVE 'A' TO INTERVALL-SW                                       
105200           MOVE JA TO ART-SW                                              
105300        ELSE                                                              
105400           PERFORM IMS-GN-WDP3A                                           
105500        END-IF                                                            
105600     END-PERFORM                                                          
105700                                                                          
105800     IF ART-SW = NEJ                                                      
105900        PERFORM IMS-GU-WDP3B                                              
106000        PERFORM UNTIL SEGMENT-SAKNAS OR LEV-SW = JA                       
106100           IF SEGMENT-FINNS                                               
106200              MOVE 'L' TO INTERVALL-SW                                    
106300              MOVE JA TO LEV-SW                                           
106400           ELSE                                                           
106500              PERFORM IMS-GN-WDP3B                                        
106600           END-IF                                                         
106700        END-PERFORM                                                       
106800                                                                          
106900        IF LEV-SW = NEJ                                                   
107000           PERFORM IMS-GU-WDP3C                                           
107100           PERFORM UNTIL SEGMENT-SAKNAS OR FKN-SW = JA                    
107200              IF SEGMENT-FINNS                                            
107300                 MOVE 'F' TO INTERVALL-SW                                 
107400                 MOVE JA TO FKN-SW                                        
107500              ELSE                                                        
107600                 PERFORM IMS-GN-WDP3C                                     
107700              END-IF                                                      
107800           END-PERFORM                                                    
107900        END-IF                                                            
108000     END-IF                                                               
108100     .                                                                    
108200     EJECT                                                                
108300 FCB-SOEK-NASTA SECTION.                                                  
108400                                                                          
108500     IF INTERVALL-ARTNR                                                   
108600        PERFORM IMS-GN-WDP3A                                              
108700        IF SEGMENT-FINNS                                                  
108800           MOVE 'A' TO INTERVALL-SW                                       
108900           MOVE JA TO ART-SW                                              
109000        ELSE                                                              
109100           PERFORM IMS-GU-WDP3B                                           
109200           IF SEGMENT-FINNS                                               
109300              MOVE 'L' TO INTERVALL-SW                                    
109400              MOVE JA TO LEV-SW                                           
109500           ELSE                                                           
109600              PERFORM IMS-GU-WDP3C                                        
109700              IF SEGMENT-FINNS                                            
109800                 MOVE 'F' TO INTERVALL-SW                                 
109900                 MOVE JA  TO FKN-SW                                       
110000              END-IF                                                      
110100           END-IF                                                         
110200        END-IF                                                            
110300     ELSE                                                                 
110400        IF INTERVALL-LEVNR                                                
110500           PERFORM IMS-GN-WDP3B                                           
110600           IF SEGMENT-FINNS                                               
110700              MOVE 'L' TO INTERVALL-SW                                    
110800              MOVE JA TO LEV-SW                                           
110900           ELSE                                                           
111000              PERFORM IMS-GU-WDP3C                                        
111100              IF SEGMENT-FINNS                                            
111200                 MOVE 'F' TO INTERVALL-SW                                 
111300                 MOVE JA TO FKN-SW                                        
111400              END-IF                                                      
111500           END-IF                                                         
111600        ELSE                                                              
111700           IF INTERVALL-FKNGRP                                            
111800              PERFORM IMS-GN-WDP3C                                        
111900              IF SEGMENT-FINNS                                            
112000                 MOVE 'F' TO INTERVALL-SW                                 
112100                 MOVE JA TO FKN-SW                                        
112200              END-IF                                                      
112300           END-IF                                                         
112400        END-IF                                                            
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 G-KOLLA-INPUT SECTION.                                                   
112900                                                                          
113000     MOVE JA    TO INDATA-SW                                              
113100     MOVE NEJ   TO INPUT-SW                                               
113200     MOVE SPACE TO UPPDATERA-SW                                           
113300                   INTERVALL-SW                                           
113400                                                                          
113500     PERFORM S01-KOLLA-INPUT                                              
113600     IF INPUT-SAKNAS                                                      
113700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
113800        CALL WMEDKONV USING MED-WMEDAREA                                  
113900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
114000        PERFORM MFS-ROER-EJ-FAELT-IN                                      
114100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
114200        MOVE NEJ TO INDATA-SW                                             
114300     ELSE                                                                 
114400        PERFORM GA-KOLLA-CMD                                              
114500        IF UPPDATERA-BORTTAG                                              
114600           CONTINUE                                                       
114700        ELSE                                                              
114800           PERFORM GB-KOLLA-IDPERSON                                      
114900           PERFORM GE-KOLLA-IDFKNGRP                                      
115000           PERFORM GD-KOLLA-LEVNR                                         
115100           PERFORM GC-KOLLA-ARTNR                                         
115200        END-IF                                                            
115300                                                                          
115400        IF INDATA-OK                                                      
115500           IF UPPDATERA-BORTTAG                                           
115600             IF MID-IDPERSON-QUAL-NY NOT = ALL '+'                        
115700                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR         
115800                MOVE NEJ TO INDATA-SW                                     
115900             END-IF                                                       
116000             IF MID-IDPERSON-PACK-NY NOT = ALL '+'                        
116100                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR         
116200                MOVE NEJ TO INDATA-SW                                     
116300             END-IF                                                       
116400             IF MID-IDARTNR-FOM-NY NOT = ALL '+'                          
116500                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-FOM-ATTR           
116600                MOVE NEJ TO INDATA-SW                                     
116700             END-IF                                                       
116800             IF MID-IDARTNR-TOM-NY NOT = ALL '+'                          
116900                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR           
117000                MOVE NEJ TO INDATA-SW                                     
117100             END-IF                                                       
117200             IF MID-IDLEVNR-NY NOT = ALL '+'                              
117300                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR               
117400                MOVE NEJ TO INDATA-SW                                     
117500             END-IF                                                       
117600             IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                         
117700                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR          
117800                MOVE NEJ TO INDATA-SW                                     
117900             END-IF                                                       
118000             IF MID-IDFKNGRP-TOM-NY NOT = ALL '+'                         
118100                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR          
118200                MOVE NEJ TO INDATA-SW                                     
118300             END-IF                                                       
118310             IF MID-IDLANDX2-NY NOT = ALL '+'                             
118320                MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLANDX2-ATTR              
118330                MOVE NEJ TO INDATA-SW                                     
118340             END-IF                                                       
118400          END-IF                                                          
118500                                                                          
118600          IF UPPDATERA-NYUPPLAGG                                          
118700             IF INTERVALL-ARTNR                                           
118800                IF MID-IDLEVNR-NY NOT = ALL '+'                           
118900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR            
119000                   MOVE NEJ TO INDATA-SW                                  
119100                END-IF                                                    
119200                IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                      
119300                   MOVE MFS-ALFA-FAELT-FEL                                
119400                                    TO MOD-IDFKNGRP-FOM-ATTR              
119500                   MOVE NEJ TO INDATA-SW                                  
119600                END-IF                                                    
119700                IF MID-IDFKNGRP-TOM-NY NOT = ALL '+'                      
119800                   MOVE MFS-ALFA-FAELT-FEL                                
119900                                    TO MOD-IDFKNGRP-TOM-ATTR              
120000                   MOVE NEJ TO INDATA-SW                                  
120100                END-IF                                                    
120200             END-IF                                                       
120300             IF INTERVALL-LEVNR                                           
120400                IF MID-IDARTNR-FOM-NY NOT = ALL '+'                       
120500                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-FOM-ATTR        
120600                   MOVE NEJ TO INDATA-SW                                  
120700                END-IF                                                    
120800                IF MID-IDARTNR-TOM-NY NOT = ALL '+'                       
120900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR        
121000                   MOVE NEJ TO INDATA-SW                                  
121100                END-IF                                                    
121200                IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                      
121300                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR        
121400                  MOVE NEJ TO INDATA-SW                                   
121500                END-IF                                                    
121600                IF MID-IDFKNGRP-TOM-NY NOT = ALL '+'                      
121700                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR        
121800                  MOVE NEJ TO INDATA-SW                                   
121900                END-IF                                                    
122000             END-IF                                                       
122100             IF INTERVALL-FKNGRP                                          
122200                IF MID-IDARTNR-FOM-NY NOT = ALL '+'                       
122300                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-FOM-ATTR        
122400                   MOVE NEJ TO INDATA-SW                                  
122500                END-IF                                                    
122600                IF MID-IDARTNR-TOM-NY NOT = ALL '+'                       
122700                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR        
122800                   MOVE NEJ TO INDATA-SW                                  
122900                END-IF                                                    
123000                IF MID-IDLEVNR-NY NOT = ALL '+'                           
123100                  MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR             
123200                  MOVE NEJ TO INDATA-SW                                   
123300                END-IF                                                    
123400             END-IF                                                       
123500          END-IF                                                          
123600                                                                          
123700          IF INDATA-OK                                                    
123800             IF UPPDATERA-BORTTAG                                         
123900                PERFORM GI-LAS-BORTTAG                                    
124000             ELSE                                                         
124100                IF UPPDATERA-NYUPPLAGG                                    
124200                   PERFORM GK-LAS-IDPERSON                                
124300                   IF INTERVALL-ARTNR                                     
124400                      PERFORM GF-LAS-IDARTNR                              
124500                   ELSE                                                   
124600                      IF INTERVALL-LEVNR                                  
124700                         PERFORM GG-LAS-IDLEVNR                           
124800                      ELSE                                                
124900                         IF INTERVALL-FKNGRP                              
125000                            PERFORM GH-LAS-IDFKNGRP                       
125100                         ELSE                                             
125200                            IF MID-IDPERSON-QUAL-NY = ALL '+'             
125300                               MOVE MFS-ALFA-FAELT-FEL                    
125400                                         TO MOD-IDPERSON-PACK-ATTR        
125500                            ELSE                                          
125600                               MOVE MFS-ALFA-FAELT-FEL                    
125700                                         TO MOD-IDPERSON-QUAL-ATTR        
125800                            END-IF                                        
125900                            MOVE NEJ TO INDATA-SW                         
126000                         END-IF                                           
126100                      END-IF                                              
126200                   END-IF                                                 
126300                ELSE                                                      
126400                   MOVE MFS-ALFA-FAELT-FEL                                
126500                                        TO MOD-IDPERSON-QUAL-ATTR         
126600                                           MOD-IDPERSON-PACK-ATTR         
126700                   MOVE NEJ TO INDATA-SW                                  
126800                END-IF                                                    
126900             END-IF                                                       
127000          END-IF                                                          
127100       END-IF                                                             
127200                                                                          
127300       IF INDATA-FEL                                                      
127400          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
127500          CALL WMEDKONV USING MED-WMEDAREA                                
127600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
127700          PERFORM MFS-ROER-EJ-FAELT-UT                                    
127800          PERFORM MFS-ROER-EJ-FAELT-IN                                    
127900       END-IF                                                             
128000     END-IF                                                               
128100     .                                                                    
128200     EJECT                                                                
128300 GA-KOLLA-CMD SECTION.                                                    
128400                                                                          
128500     MOVE +1 TO IX                                                        
128600     PERFORM UNTIL IX > IX-MAX                                            
128700        IF MID-KDCMDVAL(IX) NOT = ALL '+'                                 
128800           IF MID-KDCMDVAL(IX) = 'B' OR 'D'                               
128900              MOVE MFS-ALFA-FAELT-RAETT TO                                
129000                   MOD-KDCMDVAL-ATTR(IX)                                  
129100                                                                          
129200              INSPECT MID-IDPERSON-QUAL(IX) REPLACING LEADING             
129300                 SPACE BY ZERO                                            
129400              INSPECT MID-IDPERSON-PACK(IX) REPLACING LEADING             
129500                 SPACE BY ZERO                                            
129600                                                                          
129700              IF MID-IDPERSON-QUAL(IX) = ZERO                             
129800              AND MID-IDPERSON-PACK(IX) = ZERO                            
129900                  MOVE MFS-ALFA-FAELT-FEL TO                              
130000                    MOD-KDCMDVAL-ATTR(IX)                                 
130100                  MOVE NEJ TO INDATA-SW                                   
130200              ELSE                                                        
130300                 INSPECT MID-IDARTNR-FOM(IX) REPLACING LEADING            
130400                    SPACE BY ZERO                                         
130500                 INSPECT MID-IDARTNR-TOM(IX) REPLACING LEADING            
130600                    SPACE BY ZERO                                         
130700                 INSPECT MID-IDFKNGRP-FOM(IX) REPLACING LEADING           
130800                    SPACE BY ZERO                                         
130900                 INSPECT MID-IDFKNGRP-TOM(IX) REPLACING LEADING           
131000                    SPACE BY ZERO                                         
131100                 IF MID-IDARTNR-FOM(IX)   = ZERO                          
131200                 AND MID-IDARTNR-TOM(IX)  = ZERO                          
131300                 AND MID-IDLEVNR(IX)      = SPACE                         
131400                 AND MID-IDFKNGRP-FOM(IX) = ZERO                          
131500                 AND MID-IDFKNGRP-TOM(IX) = ZERO                          
131600                    MOVE MFS-ALFA-FAELT-FEL TO                            
131700                       MOD-KDCMDVAL-ATTR(IX)                              
131800                    MOVE NEJ TO INDATA-SW                                 
131900                 ELSE                                                     
132000                    MOVE BORTTAG TO UPPDATERA-SW                          
132100                 END-IF                                                   
132200              END-IF                                                      
132300           ELSE                                                           
132400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(IX)            
132500              MOVE NEJ TO INDATA-SW                                       
132600           END-IF                                                         
132700        END-IF                                                            
132800        ADD +1 TO IX                                                      
132900     END-PERFORM                                                          
133000     .                                                                    
133100     EJECT                                                                
133200 GB-KOLLA-IDPERSON SECTION.                                               
133300                                                                          
133400     MOVE ZERO TO WS-IDPERSON-QUAL-NY                                     
133500                  WS-IDPERSON-PACK-NY                                     
133600                                                                          
133700     IF MID-IDPERSON-QUAL-NY = ALL '+'                                    
133800        IF MID-IDPERSON-PACK-NY = ALL '+'                                 
133900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR              
134000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR              
134100           MOVE NEJ TO INDATA-SW                                          
134200        ELSE                                                              
134300           MOVE MID-IDPERSON-PACK-NY TO WS-IDPERSON-PACK-NY               
134400           INSPECT WS-IDPERSON-PACK-NY REPLACING                          
134500              LEADING SPACE BY ZERO                                       
134600           IF WS-IDPERSON-PACK-NY NUMERIC                                 
134700           AND WS-IDPERSON-PACK-NY > ZERO                                 
134800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-PACK-ATTR         
134900              MOVE NYUPPLAGG TO UPPDATERA-SW                              
135000           ELSE                                                           
135100              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR           
135200              MOVE NEJ TO INDATA-SW                                       
135300           END-IF                                                         
135400        END-IF                                                            
135500     ELSE                                                                 
135600        IF MID-IDPERSON-PACK-NY NOT = ALL '+'                             
135700           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR              
135800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR              
135900           MOVE NEJ TO INDATA-SW                                          
136000        ELSE                                                              
136100           MOVE MID-IDPERSON-QUAL-NY TO WS-IDPERSON-QUAL-NY               
136200           INSPECT WS-IDPERSON-QUAL-NY REPLACING                          
136300              LEADING SPACE BY ZERO                                       
136400           IF WS-IDPERSON-QUAL-NY NUMERIC                                 
136500           AND WS-IDPERSON-QUAL-NY > ZERO                                 
136600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPERSON-QUAL-ATTR         
136700              MOVE NYUPPLAGG TO UPPDATERA-SW                              
136800           ELSE                                                           
136900              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR           
137000              MOVE NEJ TO INDATA-SW                                       
137100           END-IF                                                         
137200        END-IF                                                            
137300     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137600 GC-KOLLA-ARTNR SECTION.                                                  
137700                                                                          
137800     MOVE ZERO TO WS-IDARTNR-FOM                                          
137900                  WS-IDARTNR-TOM                                          
138000                                                                          
138100     IF MID-IDARTNR-FOM-NY NOT = ALL '+'                                  
138200        MOVE MID-IDARTNR-FOM-NY TO WS-IDARTNR-FOM                         
138300        INSPECT WS-IDARTNR-FOM REPLACING LEADING SPACE BY ZERO            
138400        IF WS-IDARTNR-FOM NUMERIC                                         
138500        AND WS-IDARTNR-FOM > ZERO                                         
138600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-FOM-ATTR              
138700        ELSE                                                              
138800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-FOM-ATTR                
138900           MOVE NEJ TO INDATA-SW                                          
139000        END-IF                                                            
139100     END-IF                                                               
139200                                                                          
139300     IF MID-IDARTNR-TOM-NY NOT = ALL '+'                                  
139400        IF MID-IDARTNR-FOM-NY = ALL '+'                                   
139500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR                
139600           MOVE NEJ TO INDATA-SW                                          
139700        ELSE                                                              
139800           MOVE MID-IDARTNR-TOM-NY TO WS-IDARTNR-TOM                      
139900           INSPECT WS-IDARTNR-TOM REPLACING LEADING                       
140000              SPACE BY ZERO                                               
140100           IF WS-IDARTNR-TOM NUMERIC                                      
140200           AND WS-IDARTNR-TOM > ZERO                                      
140300              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-TOM-ATTR           
140400           ELSE                                                           
140500             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR              
140600             MOVE NEJ TO INDATA-SW                                        
140700          END-IF                                                          
140800        END-IF                                                            
140900     END-IF                                                               
141000                                                                          
141100     IF MID-IDARTNR-FOM-NY NOT = ALL '+'                                  
141200        IF MID-IDARTNR-TOM-NY = ALL '+'                                   
141300           MOVE MID-IDARTNR-FOM-NY TO MID-IDARTNR-TOM-NY                  
141400           MOVE WS-IDARTNR-FOM TO WS-IDARTNR-TOM                          
141500        END-IF                                                            
141600        IF WS-IDARTNR-TOM NUMERIC                                         
141700        AND WS-IDARTNR-FOM NUMERIC                                        
141800           IF WS-IDARTNR-TOM >= WS-IDARTNR-FOM                            
141900              MOVE ARTNR TO INTERVALL-SW                                  
142000           ELSE                                                           
142100              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TOM-ATTR             
142200              MOVE NEJ TO INDATA-SW                                       
142300           END-IF                                                         
142400        END-IF                                                            
142500     END-IF                                                               
142501*                                                                         
142510     IF MID-IDLANDX2-NY = WC-LAND-SE OR WC-LAND-CN OR WC-LAND-US          
142511       MOVE MID-IDLANDX2-NY      TO W-IDLAND                              
142520       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-ATTR                     
142530     ELSE                                                                 
142592       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLANDX2-ATTR                     
142593       MOVE NEJ TO INDATA-SW                                              
142596     END-IF                                                               
142600     .                                                                    
142700     EJECT                                                                
142800 GD-KOLLA-LEVNR SECTION.                                                  
142900                                                                          
143000     MOVE SPACE TO WS-IDLEVNR                                             
143100                                                                          
143200     IF MID-IDLEVNR-NY NOT = ALL '+'                                      
143300        MOVE MID-IDLEVNR-NY TO WS-IDLEVNR                                 
143400        IF WS-IDLEVNR NOT = SPACE                                         
143500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                  
143600           MOVE LEVNR TO INTERVALL-SW                                     
143700        ELSE                                                              
143800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR                    
143900           MOVE NEJ TO INDATA-SW                                          
144000        END-IF                                                            
144100     END-IF                                                               
144101*                                                                         
144110     IF MID-IDLANDX2-NY = WC-LAND-SE OR WC-LAND-CN OR WC-LAND-US          
144112       MOVE MID-IDLANDX2-NY      TO W-IDLAND                              
144113       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-ATTR                     
144114     ELSE                                                                 
144120       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLANDX2-ATTR                     
144130       MOVE NEJ TO INDATA-SW                                              
144140     END-IF                                                               
144200                                                                          
144300     .                                                                    
144400     EJECT                                                                
144500 GE-KOLLA-IDFKNGRP SECTION.                                               
144600                                                                          
144700     MOVE ZERO  TO WS-IDFKNGRP-FOM                                        
144800                   WS-IDFKNGRP-TOM                                        
144900                                                                          
145000     IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                                 
145100        MOVE MID-IDFKNGRP-FOM-NY TO WS-IDFKNGRP-FOM                       
145200        INSPECT WS-IDFKNGRP-FOM REPLACING LEADING SPACE BY ZERO           
145300        IF WS-IDFKNGRP-FOM NUMERIC                                        
145400        AND WS-IDFKNGRP-FOM > ZERO                                        
145500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFKNGRP-FOM-ATTR             
145600        ELSE                                                              
145700           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR               
145800           MOVE NEJ TO INDATA-SW                                          
145900        END-IF                                                            
146000     END-IF                                                               
146100                                                                          
146200     IF MID-IDFKNGRP-TOM-NY NOT = ALL '+'                                 
146300        IF MID-IDFKNGRP-FOM-NY = ALL '+'                                  
146400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR               
146500           MOVE NEJ TO INDATA-SW                                          
146600        ELSE                                                              
146700           MOVE MID-IDFKNGRP-TOM-NY TO WS-IDFKNGRP-TOM                    
146800           INSPECT WS-IDFKNGRP-TOM REPLACING LEADING                      
146900              SPACE BY ZERO                                               
147000           IF WS-IDFKNGRP-TOM NUMERIC                                     
147100           AND WS-IDFKNGRP-TOM > ZERO                                     
147200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFKNGRP-TOM-ATTR          
147300           ELSE                                                           
147400              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR            
147500              MOVE NEJ TO INDATA-SW                                       
147600           END-IF                                                         
147700        END-IF                                                            
147800     END-IF                                                               
147900                                                                          
148000     IF MID-IDFKNGRP-FOM-NY NOT = ALL '+'                                 
148100        IF MID-IDFKNGRP-TOM-NY = ALL '+'                                  
148200           MOVE MID-IDFKNGRP-FOM-NY TO MID-IDFKNGRP-TOM-NY                
148300           MOVE WS-IDFKNGRP-FOM TO WS-IDFKNGRP-TOM                        
148400        END-IF                                                            
148500        IF WS-IDFKNGRP-TOM NUMERIC                                        
148600        AND WS-IDFKNGRP-FOM NUMERIC                                       
148700          IF WS-IDFKNGRP-TOM >= WS-IDFKNGRP-FOM                           
148800             MOVE FKNGRP TO INTERVALL-SW                                  
148900          ELSE                                                            
149000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR             
149100             MOVE NEJ TO INDATA-SW                                        
149200          END-IF                                                          
149300        END-IF                                                            
149400     END-IF                                                               
149401*                                                                         
149410     IF MID-IDLANDX2-NY = WC-LAND-SE OR WC-LAND-CN OR WC-LAND-US          
149411       MOVE MID-IDLANDX2-NY      TO W-IDLAND                              
149420       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLANDX2-ATTR                     
149430     ELSE                                                                 
149440       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLANDX2-ATTR                     
149450       MOVE NEJ TO INDATA-SW                                              
149460     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700 GF-LAS-IDARTNR SECTION.                                                  
149800                                                                          
151200     PERFORM IMS-GET-WDP301                                               
151300     IF SEGMENT-SAKNAS                                                    
151400        MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                             
151500        CALL WMEDKONV USING MED-WMEDAREA                                  
151600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
151700        MOVE NEJ TO INDATA-SW                                             
151800     ELSE                                                                 
151900        PERFORM IMS-GNP-WDP311                                            
152000        IF SEGMENT-FINNS                                                  
152100           PERFORM UNTIL SEGMENT-SAKNAS                                   
152200              MOVE PERS-IDPERSON TO W-IDPERSON                            
152300              PERFORM IMS-GET-WDP311-PCB2                                 
152400              PERFORM IMS-SOEK-WDP321-PCB2                                
152500              MOVE NEJ TO TRAEFF-SW                                       
152600              IF SEGMENT-SAKNAS                                           
152700                 CONTINUE                                                 
152800              ELSE                                                        
152900                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
152910                   IF IART-IDLANDX2 = MID-IDLANDX2-NY                     
153000                     IF WS-IDARTNR-FOM-NUM > IART-IDARTNR-TOM             
153100                        PERFORM IMS-SOEK-WDP321-PCB2                      
153200                     ELSE                                                 
153300                        MOVE JA TO TRAEFF-SW                              
153400                     END-IF                                               
153410                   ELSE                                                   
153411                     PERFORM IMS-SOEK-WDP321-PCB2                         
153420                   END-IF                                                 
153500                   IF TRAEFF-SW = JA                                      
153600                      IF WS-IDARTNR-FOM-NUM < IART-IDARTNR-FOM            
153700                         IF WS-IDARTNR-TOM-NUM < IART-IDARTNR-FOM         
153800                            CONTINUE                                      
153900                         ELSE                                             
154000                            MOVE MFS-ALFA-FAELT-FEL TO                    
154100                                          MOD-IDARTNR-FOM-ATTR            
154200                                          MOD-IDARTNR-TOM-ATTR            
154300                            MOVE NEJ TO INDATA-SW                         
154400                            MOVE JA TO TRAEFF-SW                          
154500                         END-IF                                           
154600                      ELSE                                                
154700                         MOVE MFS-ALFA-FAELT-FEL TO                       
154800                                          MOD-IDARTNR-FOM-ATTR            
154900                                          MOD-IDARTNR-TOM-ATTR            
155000                         MOVE NEJ TO INDATA-SW                            
155100                         MOVE JA TO TRAEFF-SW                             
155200                      END-IF                                              
155300                   END-IF                                                 
155400                 END-PERFORM                                              
155500              END-IF                                                      
155600              PERFORM IMS-GNP-WDP311                                      
155700           END-PERFORM                                                    
155800        END-IF                                                            
155900     END-IF                                                               
156000     .                                                                    
156100     EJECT                                                                
156200 GG-LAS-IDLEVNR SECTION.                                                  
156300                                                                          
156400     PERFORM IMS-GET-WDP301                                               
156500     IF SEGMENT-SAKNAS                                                    
156600        MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                             
156700        CALL WMEDKONV USING MED-WMEDAREA                                  
156800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
156900        MOVE NEJ TO INDATA-SW                                             
157000     ELSE                                                                 
157100        PERFORM IMS-GNP-WDP311                                            
157200        IF SEGMENT-FINNS                                                  
157300           PERFORM UNTIL SEGMENT-SAKNAS                                   
157400              MOVE PERS-IDPERSON TO W-IDPERSON                            
157500              MOVE WS-IDLEVNR TO W-IDLEVNR22-MIN                          
157510                                 W-IDLEVNR22-MAX                          
157600              PERFORM IMS-GET-WDP311-PCB2                                 
157700              PERFORM IMS-GNP-WDP322-PCB2                                 
157800              IF SEGMENT-FINNS                                            
157900                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-ATTR              
158000                 MOVE NEJ TO INDATA-SW                                    
158100              END-IF                                                      
158200              PERFORM IMS-GNP-WDP311                                      
158300           END-PERFORM                                                    
158400        END-IF                                                            
158500     END-IF                                                               
158600     .                                                                    
158700     EJECT                                                                
158800 GH-LAS-IDFKNGRP SECTION.                                                 
158900                                                                          
159000     PERFORM IMS-GET-WDP301                                               
159100     IF SEGMENT-SAKNAS                                                    
159200        MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                             
159300        CALL WMEDKONV USING MED-WMEDAREA                                  
159400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
159500        MOVE NEJ TO INDATA-SW                                             
159600     ELSE                                                                 
159700        PERFORM IMS-GNP-WDP311                                            
159800        IF SEGMENT-FINNS                                                  
159900           PERFORM UNTIL SEGMENT-SAKNAS                                   
160000              MOVE PERS-IDPERSON TO W-IDPERSON                            
160100              MOVE NEJ TO TRAEFF-SW                                       
160200              PERFORM IMS-GET-WDP311-PCB2                                 
160300              PERFORM IMS-SOEK-WDP323-PCB2                                
160400              IF SEGMENT-SAKNAS                                           
160500                 CONTINUE                                                 
160600              ELSE                                                        
160700                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
160710                   IF IFKN-IDLANDX2 = MID-IDLANDX2-NY                     
160800                     IF WS-IDFKNGRP-FOM-NUM > IFKN-IDFKNGRP-TOM           
160900                        PERFORM IMS-SOEK-WDP323-PCB2                      
161000                     ELSE                                                 
161100                        MOVE JA TO TRAEFF-SW                              
161200                     END-IF                                               
161201                   ELSE                                                   
161202                     PERFORM IMS-SOEK-WDP323-PCB2                         
161210                   END-IF                                                 
161300                   IF TRAEFF-SW = JA                                      
161400                      IF WS-IDFKNGRP-FOM-NUM < IFKN-IDFKNGRP-FOM          
161500                        IF WS-IDFKNGRP-TOM-NUM < IFKN-IDFKNGRP-FOM        
161600                           CONTINUE                                       
161700                        ELSE                                              
161800                           MOVE MFS-ALFA-FAELT-FEL TO                     
161900                                           MOD-IDFKNGRP-FOM-ATTR          
162000                                           MOD-IDFKNGRP-TOM-ATTR          
162100                           MOVE NEJ TO INDATA-SW                          
162200                           MOVE JA TO TRAEFF-SW                           
162300                         END-IF                                           
162400                      ELSE                                                
162500                         MOVE MFS-ALFA-FAELT-FEL TO                       
162600                                           MOD-IDFKNGRP-FOM-ATTR          
162700                                           MOD-IDFKNGRP-TOM-ATTR          
162800                         MOVE NEJ TO INDATA-SW                            
162900                         MOVE JA TO TRAEFF-SW                             
163000                      END-IF                                              
163100                   END-IF                                                 
163200                 END-PERFORM                                              
163300              END-IF                                                      
163400              PERFORM IMS-GNP-WDP311                                      
163500           END-PERFORM                                                    
163600        END-IF                                                            
163700     END-IF                                                               
163800     .                                                                    
163900     EJECT                                                                
164000 GI-LAS-BORTTAG SECTION.                                                  
164100                                                                          
164200     MOVE +1 TO IX                                                        
164300     PERFORM UNTIL IX > IX-MAX                                            
164400        IF MID-KDCMDVAL(IX) = 'B' OR 'D'                                  
164500           INSPECT MID-IDPERSON-QUAL(IX)                                  
164600                               REPLACING LEADING SPACE BY ZERO            
164700           IF MID-IDPERSON-QUAL(IX)= ZERO                                 
164800              INSPECT MID-IDPERSON-PACK(IX)                               
164900                               REPLACING LEADING SPACE BY ZERO            
165000              IF MID-IDPERSON-PACK(IX)= ZERO                              
165100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR(IX)         
165200                 MOVE NEJ TO INDATA-SW                                    
165300              END-IF                                                      
165400           END-IF                                                         
165500                                                                          
165600           IF INDATA-OK                                                   
165700              INSPECT MID-IDARTNR-FOM(IX)                                 
165800                             REPLACING LEADING SPACE BY ZERO              
165900              INSPECT MID-IDARTNR-TOM(IX)                                 
166000                             REPLACING LEADING SPACE BY ZERO              
166100              IF MID-IDARTNR-FOM(IX) = ZERO                               
166200              AND MID-IDARTNR-TOM(IX) = ZERO                              
166300                 IF MID-IDLEVNR(IX) = SPACE                               
166400                    INSPECT MID-IDFKNGRP-FOM(IX)                          
166500                             REPLACING LEADING SPACE BY ZERO              
166600                    INSPECT MID-IDFKNGRP-TOM(IX)                          
166700                             REPLACING LEADING SPACE BY ZERO              
166800                                                                          
166900                    IF MID-IDARTNR-FOM(IX) = ZERO                         
167000                    AND MID-IDARTNR-TOM(IX) = ZERO                        
167100                    AND MID-IDLEVNR(IX) = SPACE                           
167200                    AND MID-IDFKNGRP-FOM(IX) = ZERO                       
167300                    AND MID-IDFKNGRP-TOM(IX) = ZERO                       
167400                       MOVE MFS-ALFA-FAELT-FEL                            
167500                                       TO MOD-KDCMDVAL-ATTR(IX)           
167600                       MOVE NEJ TO INDATA-SW                              
167700                    ELSE                                                  
167800                       IF MID-IDPERSON-QUAL(IX) = ZERO                    
167900                          MOVE MID-IDPERSON-PACK(IX) TO W-IDPERSON        
168000                       ELSE                                               
168100                          MOVE MID-IDPERSON-QUAL(IX) TO W-IDPERSON        
168200                       END-IF                                             
168300                       MOVE MID-IDFKNGRP-FOM(IX) TO                       
168400                                               W-IDFKNGRPF-MIN            
168500                                               W-IDFKNGRPF-MAX            
168600                       MOVE MID-IDFKNGRP-TOM(IX) TO                       
168700                                               W-IDFKNGRPT-MIN            
168800                                               W-IDFKNGRPT-MAX            
168810                       MOVE WS-IDLANDX2          TO                       
168820                                                W-IDLAND23-MIN            
168830                                                W-IDLAND23-MAX            
168900                       PERFORM IMS-GHU-WDP323                             
169000                       IF SEGMENT-SAKNAS                                  
169100                          MOVE MFS-ALFA-FAELT-FEL                         
169200                                        TO MOD-KDCMDVAL-ATTR(IX)          
169300                          MOVE NEJ TO INDATA-SW                           
169400                       END-IF                                             
169500                    END-IF                                                
169600                 ELSE                                                     
169700                    IF MID-IDPERSON-QUAL(IX) = ZERO                       
169800                       MOVE MID-IDPERSON-PACK(IX) TO W-IDPERSON           
169900                    ELSE                                                  
170000                       MOVE MID-IDPERSON-QUAL(IX) TO W-IDPERSON           
170100                    END-IF                                                
170200                    MOVE MID-IDLEVNR(IX) TO W-IDLEVNR22-MIN               
170210                                            W-IDLEVNR22-MAX               
170220                    MOVE WS-IDLANDX2     TO W-IDLAND22-MIN                
170230                                            W-IDLAND22-MAX                
170300                    PERFORM IMS-GHU-WDP322                                
170400                    IF SEGMENT-SAKNAS                                     
170500                       MOVE MFS-ALFA-FAELT-FEL                            
170600                                    TO MOD-KDCMDVAL-ATTR(IX)              
170700                       MOVE NEJ TO INDATA-SW                              
170800                    END-IF                                                
170900                 END-IF                                                   
171000              ELSE                                                        
171100                 IF MID-IDPERSON-QUAL(IX) = ZERO                          
171200                    MOVE MID-IDPERSON-PACK(IX) TO W-IDPERSON              
171300                 ELSE                                                     
171400                    MOVE MID-IDPERSON-QUAL(IX) TO W-IDPERSON              
171500                 END-IF                                                   
171600                 MOVE MID-IDARTNR-FOM(IX) TO W-IDARTNRF-MIN               
171700                                             W-IDARTNRF-MAX               
171800                 MOVE MID-IDARTNR-TOM(IX) TO W-IDARTNRT-MIN               
171900                                             W-IDARTNRT-MAX               
171910                 MOVE WS-IDLANDX2         TO W-IDLAND21-MIN               
171920                                             W-IDLAND21-MAX               
172000                 PERFORM IMS-GHU-WDP321                                   
172100                 IF SEGMENT-SAKNAS                                        
172200                    MOVE MFS-ALFA-FAELT-FEL                               
172300                                    TO MOD-KDCMDVAL-ATTR(IX)              
172400                    MOVE NEJ TO INDATA-SW                                 
172500                 END-IF                                                   
172600              END-IF                                                      
172700           END-IF                                                         
172800        END-IF                                                            
172900        ADD +1 TO IX                                                      
173000     END-PERFORM                                                          
173100     .                                                                    
173200     EJECT                                                                
173300 GK-LAS-IDPERSON SECTION.                                                 
173400                                                                          
173500     IF WS-IDPERSON-QUAL-NY NOT = ZERO                                    
173600        IF MSGI-KDARBTYP = 'QUAL'                                         
173700        AND WS-IDPERSON-QUAL-NY = MSGI-IDPERSON-QUAL                      
173800           MOVE MSGI-IDPERSON-QUAL TO W-IDPERSON                          
173900           PERFORM GKA-LAS-IDPERSON                                       
174000        ELSE                                                              
174100          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR               
174200          MOVE NEJ TO INDATA-SW                                           
174300        END-IF                                                            
174400     ELSE                                                                 
174500        IF MSGI-KDARBTYP = 'CDC'                                          
174600        AND WS-IDPERSON-PACK-NY = MSGI-IDPERSON-CDC                       
174700           MOVE MSGI-IDPERSON-CDC TO W-IDPERSON                           
174800           PERFORM GKA-LAS-IDPERSON                                       
174900        ELSE                                                              
175000          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR               
175100          MOVE NEJ TO INDATA-SW                                           
175200        END-IF                                                            
175300     END-IF                                                               
175400     .                                                                    
175500     EJECT                                                                
175600 GKA-LAS-IDPERSON SECTION.                                                
175700                                                                          
175800     MOVE MSGI-KDARBTYP TO W-KDARBTYP                                     
175900     PERFORM IMS-GET-WDP311                                               
176000     IF SEGMENT-SAKNAS                                                    
176100       IF W-KDARBTYP = 'CDC'                                              
176200          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-PACK-ATTR               
176300       ELSE                                                               
176400          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPERSON-QUAL-ATTR               
176500       END-IF                                                             
176600       MOVE NEJ TO INDATA-SW                                              
176700     END-IF                                                               
176800     .                                                                    
176900     EJECT                                                                
177000 H-UPPDATERA SECTION.                                                     
177100                                                                          
177200     MOVE MSGI-KDARBTYP TO W-KDARBTYP                                     
177300     IF W-KDARBTYP = 'CDC'                                                
177400        MOVE MSGI-IDPERSON-CDC TO W-IDPERSON                              
177500     ELSE                                                                 
177600        MOVE MSGI-IDPERSON-QUAL TO W-IDPERSON                             
177700     END-IF                                                               
177800                                                                          
177900     IF UPPDATERA-BORTTAG                                                 
178000        PERFORM HA-BORTTAG                                                
178100     ELSE                                                                 
178200        IF UPPDATERA-NYUPPLAGG                                            
178300           PERFORM HB-NYUPPLAGG                                           
178400        END-IF                                                            
178500     END-IF                                                               
178600                                                                          
178700******** ÅTERSTÄLL NYCKLAR                                                
178800     MOVE MSGI-KDARBTYP TO W-KDARBTYP                                     
178900     IF W-KDARBTYP = 'CDC'                                                
179000        MOVE MSGI-IDPERSON-CDC TO W-IDPERSON                              
179100     ELSE                                                                 
179200        MOVE MSGI-IDPERSON-QUAL TO W-IDPERSON                             
179300     END-IF                                                               
179400     MOVE LOW-VALUE TO  W-WDP321KY-MIN-X                                  
179410                        W-WDP322KY-MIN-X                                  
179500                        W-WDP323KY-MIN-X                                  
179600     MOVE HIGH-VALUE TO W-WDP321KY-MAX-X                                  
179700                        W-WDP322KY-MAX-X                                  
179800                        W-WDP323KY-MAX-X                                  
179910                                                                          
179930     MOVE WS-IDLANDX2 TO W-IDLAND21-MIN                                   
179931                         W-IDLAND21-MAX                                   
179932                         W-IDLAND22-MIN                                   
179933                         W-IDLAND22-MAX                                   
179934                         W-IDLAND23-MIN                                   
179935                         W-IDLAND23-MAX                                   
179960                                                                          
180000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
180100     CALL WMEDKONV USING MED-WMEDAREA                                     
180200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
180300     PERFORM MFS-FORM-ATTR                                                
180400     PERFORM MFS-RENSA-FAELT-IN                                           
180500     .                                                                    
180600     EJECT                                                                
180700 HA-BORTTAG SECTION.                                                      
180800                                                                          
180900     PERFORM IMS-GET-WDP301                                               
181000                                                                          
181100     MOVE +1 TO IX                                                        
181200     PERFORM UNTIL IX > IX-MAX                                            
181300        IF MID-KDCMDVAL(IX) = 'B' OR 'D'                                  
181400           IF MID-IDPERSON-QUAL(IX) = ZERO                                
181500              MOVE MID-IDPERSON-PACK(IX) TO W-IDPERSON                    
181600           ELSE                                                           
181700              MOVE MID-IDPERSON-QUAL(IX) TO W-IDPERSON                    
181800           END-IF                                                         
181900           IF MID-IDARTNR-FOM(IX) = ZERO                                  
182000           AND MID-IDARTNR-TOM(IX) = ZERO                                 
182100              IF MID-IDLEVNR(IX) = SPACE                                  
182200                 MOVE MID-IDFKNGRP-FOM(IX) TO W-IDFKNGRPF-MIN             
182300                                              W-IDFKNGRPF-MAX             
182400                 MOVE MID-IDFKNGRP-TOM(IX) TO W-IDFKNGRPT-MIN             
182500                                              W-IDFKNGRPT-MAX             
182510                 MOVE WS-IDLANDX2          TO W-IDLAND23-MIN              
182520                                              W-IDLAND23-MAX              
182600                 PERFORM IMS-GHU-WDP323                                   
182700                 PERFORM IMS-DLET-WDP323                                  
182800              ELSE                                                        
182900                 MOVE MID-IDLEVNR(IX) TO W-IDLEVNR22-MIN                  
182901                                         W-IDLEVNR22-MAX                  
182910                 MOVE WS-IDLANDX2     TO W-IDLAND22-MIN                   
182920                                         W-IDLAND22-MAX                   
183000                 PERFORM IMS-GHU-WDP322                                   
183100                 PERFORM IMS-DLET-WDP322                                  
183200              END-IF                                                      
183300           ELSE                                                           
183400              MOVE MID-IDARTNR-FOM(IX) TO W-IDARTNRF-MIN                  
183500                                          W-IDARTNRF-MAX                  
183600              MOVE MID-IDARTNR-TOM(IX) TO W-IDARTNRT-MIN                  
183700                                          W-IDARTNRT-MAX                  
183710              MOVE WS-IDLANDX2         TO W-IDLAND21-MIN                  
183720                                          W-IDLAND21-MAX                  
183800              PERFORM IMS-GHU-WDP321                                      
183900              PERFORM IMS-DLET-WDP321                                     
184000           END-IF                                                         
184100        END-IF                                                            
184200        ADD +1 TO IX                                                      
184300     END-PERFORM                                                          
184400     .                                                                    
184500     EJECT                                                                
184600 HB-NYUPPLAGG SECTION.                                                    
184700                                                                          
184800     PERFORM IMS-GET-WDP311                                               
184900     IF SEGMENT-FINNS                                                     
185000        IF INTERVALL-ARTNR                                                
185100           INSPECT MID-IDARTNR-FOM-NY                                     
185200                              REPLACING LEADING SPACE BY ZERO             
185300           INSPECT MID-IDARTNR-TOM-NY                                     
185400                              REPLACING LEADING SPACE BY ZERO             
185500           MOVE MID-IDARTNR-FOM-NY TO IART-IDARTNR-FOM                    
185600           MOVE MID-IDARTNR-TOM-NY TO IART-IDARTNR-TOM                    
185601           IF MID-IDLANDX2-NY = WC-LAND-CN                                
185610             MOVE WC-LAND-CN         TO IART-IDLANDX2                     
185611           ELSE                                                           
185612             IF MID-IDLANDX2-NY = WC-LAND-US                              
185613               MOVE WC-LAND-US       TO IART-IDLANDX2                     
185614             ELSE                                                         
185620               MOVE WC-LAND-SE       TO IART-IDLANDX2                     
185630             END-IF                                                       
185640           END-IF                                                         
185700           PERFORM IMS-ISRT-WDP321                                        
185800        ELSE                                                              
185900           IF INTERVALL-LEVNR                                             
186000              MOVE MID-IDLEVNR-NY TO ILEV-IDLEVNR                         
186001              IF MID-IDLANDX2-NY = WC-LAND-CN                             
186002                MOVE WC-LAND-CN       TO ILEV-IDLANDX2                    
186003              ELSE                                                        
186004                IF MID-IDLANDX2-NY = WC-LAND-US                           
186005                  MOVE WC-LAND-US     TO ILEV-IDLANDX2                    
186006                ELSE                                                      
186007                  MOVE WC-LAND-SE     TO ILEV-IDLANDX2                    
186008                END-IF                                                    
186009              END-IF                                                      
186100              PERFORM IMS-ISRT-WDP322                                     
186200           ELSE                                                           
186300              IF INTERVALL-FKNGRP                                         
186400                 INSPECT MID-IDFKNGRP-FOM-NY                              
186500                                   REPLACING LEADING SPACE BY ZERO        
186600                 INSPECT MID-IDFKNGRP-TOM-NY                              
186700                                   REPLACING LEADING SPACE BY ZERO        
186800                 MOVE MID-IDFKNGRP-FOM-NY TO IFKN-IDFKNGRP-FOM            
186900                 MOVE MID-IDFKNGRP-TOM-NY TO IFKN-IDFKNGRP-TOM            
186901                 IF MID-IDLANDX2-NY = WC-LAND-CN                          
186902                   MOVE WC-LAND-CN         TO IFKN-IDLANDX2               
186903                 ELSE                                                     
186904                   IF MID-IDLANDX2-NY = WC-LAND-US                        
186905                     MOVE WC-LAND-US       TO IFKN-IDLANDX2               
186906                   ELSE                                                   
186907                     MOVE WC-LAND-SE       TO IFKN-IDLANDX2               
186908                   END-IF                                                 
186909                 END-IF                                                   
187000                 PERFORM IMS-ISRT-WDP323                                  
187100              END-IF                                                      
187200           END-IF                                                         
187300        END-IF                                                            
187400     END-IF                                                               
187500     .                                                                    
187600     EJECT                                                                
187700 S01-KOLLA-INPUT SECTION.                                                 
187800                                                                          
187900     MOVE +1 TO IX                                                        
188000     PERFORM UNTIL IX > IX-MAX                                            
188100       IF MID-KDCMDVAL(IX) = ALL '+'                                      
188200          CONTINUE                                                        
188300       ELSE                                                               
188400          MOVE JA TO INPUT-SW                                             
188500       END-IF                                                             
188600       ADD +1 TO IX                                                       
188700     END-PERFORM                                                          
188800                                                                          
188900     IF INPUT-SAKNAS                                                      
189000        IF MID-IDPERSON-QUAL-NY = ALL '+'                                 
189100        AND MID-IDPERSON-PACK-NY = ALL '+'                                
189200        AND MID-IDARTNR-FOM-NY = ALL '+'                                  
189300        AND MID-IDARTNR-TOM-NY = ALL '+'                                  
189400        AND MID-IDLEVNR-NY = ALL '+'                                      
189500        AND MID-IDFKNGRP-FOM-NY = ALL '+'                                 
189600        AND MID-IDFKNGRP-TOM-NY = ALL '+'                                 
189610        AND MID-IDLANDX2-NY = ALL '+'                                     
189700           CONTINUE                                                       
189800        ELSE                                                              
189900           MOVE JA TO INPUT-SW                                            
190000        END-IF                                                            
190100     END-IF                                                               
190200     .                                                                    
190300     EJECT                                                                
196400 MFS-RENSA-FAELT-UT SECTION.                                              
196500                                                                          
196600     MOVE +1 TO IX                                                        
196700     PERFORM UNTIL IX > IX-MAX                                            
196800        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
196900        ADD +1 TO IX                                                      
197000     END-PERFORM                                                          
197100     .                                                                    
197200     SKIP3                                                                
197300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
197400                                                                          
197500     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(IX)                             
197600                             MOD-IDPERSON-QUAL(IX)                        
197700                             MOD-IDPERSON-PACK(IX)                        
197800                             MOD-IDARTNR-FOM(IX)                          
197900                             MOD-IDARTNR-TOM(IX)                          
198000                             MOD-IDLEVNR(IX)                              
198100                             MOD-IDFKNGRP-FOM(IX)                         
198200                             MOD-IDFKNGRP-TOM(IX)                         
198300     .                                                                    
198400     EJECT                                                                
198500 MFS-RENSA-SPAR SECTION.                                                  
198600     MOVE MFS-RENSA-FAELT TO SPAR-KDARBTYP-ENTER                          
198700                             SPAR-KDARBTYP-NEXT                           
198800                             SPAR-IDPERSON-ENTER                          
198900                             SPAR-IDPERSON-NEXT                           
199000                             SPAR-IDARTNR-FOM-ENTER                       
199100                             SPAR-IDARTNR-TOM-ENTER                       
199200                             SPAR-IDARTNR-FOM-NEXT                        
199300                             SPAR-IDARTNR-TOM-NEXT                        
199400                             SPAR-IDLEVNR-ENTER                           
199500                             SPAR-IDLEVNR-NEXT                            
199600                             SPAR-IDFKNGRP-FOM-ENTER                      
199700                             SPAR-IDFKNGRP-TOM-ENTER                      
199800                             SPAR-IDFKNGRP-FOM-NEXT                       
199900                             SPAR-IDFKNGRP-TOM-NEXT                       
199910                             SPAR-IDLANDX2-ENTER                          
199920                             SPAR-IDLANDX2-NEXT                           
200000     .                                                                    
200100     EJECT                                                                
200200 MFS-RENSA-FAELT-IN SECTION.                                              
200300                                                                          
200400     MOVE +1 TO IX                                                        
200500     PERFORM UNTIL IX > IX-MAX                                            
200600        MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL(IX)                          
200700        ADD +1 TO IX                                                      
200800     END-PERFORM                                                          
200900                                                                          
201000     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-QUAL-NY                         
201100                             MOD-IDPERSON-PACK-NY                         
201200                             MOD-IDARTNR-FOM-NY                           
201300                             MOD-IDARTNR-TOM-NY                           
201400                             MOD-IDLEVNR-NY                               
201500                             MOD-IDFKNGRP-FOM-NY                          
201600                             MOD-IDFKNGRP-TOM-NY                          
201610                             MOD-IDLANDX2-NY                              
201700     .                                                                    
201800     SKIP2                                                                
201900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
202000                                                                          
202100     MOVE +1 TO IX                                                        
202200     PERFORM UNTIL IX > IX-MAX                                            
202300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
202400       ADD +1 TO IX                                                       
202500     END-PERFORM                                                          
202600     .                                                                    
202700     EJECT                                                                
202800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
202900                                                                          
203000     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL(IX)                           
203100                               MOD-IDPERSON-QUAL(IX)                      
203200                               MOD-IDPERSON-PACK(IX)                      
203300                               MOD-IDARTNR-FOM(IX)                        
203400                               MOD-IDARTNR-TOM(IX)                        
203500                               MOD-IDLEVNR(IX)                            
203600                               MOD-IDFKNGRP-FOM(IX)                       
203700                               MOD-IDFKNGRP-TOM(IX)                       
203800     .                                                                    
203900     SKIP3                                                                
204000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
204100                                                                          
204200     MOVE +1 TO IX                                                        
204300     PERFORM UNTIL IX > IX-MAX                                            
204400        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL(IX)                        
204500        ADD +1 TO IX                                                      
204600     END-PERFORM                                                          
204700                                                                          
204800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON-QUAL-NY                       
204900                               MOD-IDPERSON-PACK-NY                       
205000                               MOD-IDARTNR-FOM-NY                         
205100                               MOD-IDARTNR-TOM-NY                         
205200                               MOD-IDLEVNR-NY                             
205300                               MOD-IDFKNGRP-FOM-NY                        
205400                               MOD-IDFKNGRP-TOM-NY                        
205410                               MOD-IDLANDX2-NY                            
205500     .                                                                    
205600     EJECT                                                                
205700 MFS-FORM-ATTR SECTION.                                                   
205800                                                                          
205900     MOVE +1 TO IX                                                        
206000     PERFORM UNTIL IX > IX-MAX                                            
206100        MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL(IX)                       
206200        ADD +1 TO IX                                                      
206300     END-PERFORM                                                          
206400                                                                          
206500     MOVE MFS-FORMATETS-ATTR TO MOD-IDPERSON-QUAL-NY                      
206600                                MOD-IDPERSON-PACK-NY                      
206700                                MOD-IDARTNR-FOM-NY                        
206800                                MOD-IDARTNR-TOM-NY                        
206900                                MOD-IDLEVNR-NY                            
207000                                MOD-IDFKNGRP-FOM-NY                       
207100                                MOD-IDFKNGRP-TOM-NY                       
207110                                MOD-IDLANDX2-NY                           
207200     .                                                                    
207300     EJECT                                                                
207400* --- IMS SEKTIONER ---                                                   
207500*                                                                         
207600 IMS-GET-MSG SECTION.                                                     
207700     MOVE '  QC' TO GODK-STATUSKODER                                      
207800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
207900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     SKIP3                                                                
208300 IMS-INSERT-MSG SECTION.                                                  
208400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
208500     MOVE SPACE TO GODK-STATUSKODER                                       
208600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
208700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000     EJECT                                                                
209100 IMS-GET-WDK601 SECTION.                                                  
209200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
209300          DELIMITED BY SIZE INTO SSA1                                     
209400     MOVE '  GE' TO GODK-STATUSKODER                                      
209500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
209600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
209700     PERFORM IMS-STATUSKONTROLL                                           
209800     .                                                                    
209900     SKIP3                                                                
210900 IMS-GET-WDP301 SECTION.                                                  
211000     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
211100            DELIMITED BY SIZE INTO SSA1                                   
211200     MOVE '  GE' TO GODK-STATUSKODER                                      
211300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP301 SSA1                    
211400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
211500     PERFORM IMS-STATUSKONTROLL                                           
211600     .                                                                    
211700     SKIP3                                                                
211800 IMS-GET-WDP311 SECTION.                                                  
211900     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
212000            DELIMITED BY SIZE INTO SSA1                                   
212100     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
212200            DELIMITED BY SIZE INTO SSA2                                   
212300     MOVE '  GE' TO GODK-STATUSKODER                                      
212400     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
212500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
212600     PERFORM IMS-STATUSKONTROLL                                           
212700     .                                                                    
212800     SKIP3                                                                
212900 IMS-GET-WDP311-PCB2 SECTION.                                             
213000     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
213100            DELIMITED BY SIZE INTO SSA1                                   
213200     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
213300            DELIMITED BY SIZE INTO SSA2                                   
213400     MOVE '  GE' TO GODK-STATUSKODER                                      
213500     CALL CBLTDLI USING GU WDP32-PCB DLI-IO-WDP311 SSA1 SSA2              
213600     MOVE WDP32-STATUS-CODE TO STATUS-WS                                  
213700     PERFORM IMS-STATUSKONTROLL                                           
213800     .                                                                    
213900     EJECT                                                                
214000 IMS-GNP-WDP311 SECTION.                                                  
214100     MOVE 'WDP311   ' TO SSA1                                             
214200     MOVE '  GE' TO GODK-STATUSKODER                                      
214300     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP311 SSA1                   
214400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     SKIP3                                                                
214800 IMS-GNP-WDP311-FIRST SECTION.                                            
214900     STRING 'WDP311  (IDPERSON>=' W-IDPERSON-X ')'                        
215000            DELIMITED BY SIZE INTO SSA1                                   
215100     MOVE '  GE' TO GODK-STATUSKODER                                      
215200     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP311 SSA1                   
215300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
215400     PERFORM IMS-STATUSKONTROLL                                           
215500     .                                                                    
215600     SKIP3                                                                
215700 IMS-GET-WDP321 SECTION.                                                  
215800     STRING 'WDP321  (WDP321KY>=' W-WDP321KY-MIN-X                        
215900                    '&WDP321KY=<' W-WDP321KY-MAX-X ')'                    
216000            DELIMITED BY SIZE INTO SSA1                                   
216100     MOVE '  GE' TO GODK-STATUSKODER                                      
216200     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP321 SSA1                   
216300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     EJECT                                                                
217700 IMS-GHU-WDP321 SECTION.                                                  
217800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
217900            DELIMITED BY SIZE INTO SSA1                                   
218000     STRING 'WDP321  (WDP321KY>=' W-WDP321KY-MIN-X                        
218100                    '&WDP321KY=<' W-WDP321KY-MAX-X ')'                    
218200            DELIMITED BY SIZE INTO SSA2                                   
218300     MOVE '  GE' TO GODK-STATUSKODER                                      
218400     CALL CBLTDLI USING GHU WDP3-PCB DLI-IO-WDP321 SSA1 SSA2              
218500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     SKIP3                                                                
218900 IMS-SOEK-WDP321-PCB2 SECTION.                                            
219000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
219100            DELIMITED BY SIZE INTO SSA1                                   
219110**   STRING 'WDP321  (IDLAND   =' W-IDLAND-X ')'                          
219120**          DELIMITED BY SIZE INTO SSA2                                   
219200     MOVE 'WDP321 ' TO SSA2                                               
219300     MOVE '  GE' TO GODK-STATUSKODER                                      
219400     CALL CBLTDLI USING GNP WDP32-PCB DLI-IO-WDP321 SSA1 SSA2             
219500     MOVE WDP32-STATUS-CODE TO STATUS-WS                                  
219600     PERFORM IMS-STATUSKONTROLL                                           
219700     .                                                                    
219800     EJECT                                                                
219900 IMS-GET-WDP322 SECTION.                                                  
220000     STRING 'WDP322  (WDP322KY>=' W-WDP322KY-MIN-X                        
220010                    '&WDP322KY=<' W-WDP322KY-MAX-X ')'                    
220100            DELIMITED BY SIZE INTO SSA1                                   
220200     MOVE '  GE' TO GODK-STATUSKODER                                      
220300     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP322 SSA1                   
220400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
220500     PERFORM IMS-STATUSKONTROLL                                           
220600     .                                                                    
220700     SKIP3                                                                
221700 IMS-GHU-WDP322 SECTION.                                                  
221800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
221900            DELIMITED BY SIZE INTO SSA1                                   
222000     STRING 'WDP322  (WDP322KY>=' W-WDP322KY-MIN-X                        
222010                    '&WDP322KY=<' W-WDP322KY-MAX-X ')'                    
222100            DELIMITED BY SIZE INTO SSA2                                   
222200     MOVE '  GE' TO GODK-STATUSKODER                                      
222300     CALL CBLTDLI USING GHU WDP3-PCB DLI-IO-WDP322 SSA1 SSA2              
222400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
222500     PERFORM IMS-STATUSKONTROLL                                           
222600     .                                                                    
222700     EJECT                                                                
222800 IMS-GNP-WDP322-PCB2 SECTION.                                             
222900     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
223000            DELIMITED BY SIZE INTO SSA1                                   
223100**   STRING 'WDP322  (WDP322KY>=' W-WDP322KY-MIN-X                        
223110**                  '&WDP322KY=<' W-WDP322KY-MAX-X ')'                    
223111**          DELIMITED BY SIZE INTO SSA2                                   
223310     STRING 'WDP322  (IDLEVNR  =' WS-IDLEVNR                              
223311                    '&IDLAND  = ' W-IDLAND-X ')'                          
223320            DELIMITED BY SIZE INTO SSA2                                   
223330     MOVE '  GE' TO GODK-STATUSKODER                                      
223400     CALL CBLTDLI USING GNP WDP32-PCB DLI-IO-WDP322 SSA1 SSA2             
223500     MOVE WDP32-STATUS-CODE TO STATUS-WS                                  
223600     PERFORM IMS-STATUSKONTROLL                                           
223700     .                                                                    
223800     SKIP3                                                                
223900 IMS-GET-WDP323 SECTION.                                                  
224000     STRING 'WDP323  (WDP323KY>=' W-WDP323KY-MIN-X                        
224100                    '&WDP323KY=<' W-WDP323KY-MAX-X ')'                    
224200            DELIMITED BY SIZE INTO SSA1                                   
224300     MOVE '  GE' TO GODK-STATUSKODER                                      
224400     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP323 SSA1                   
224500     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
224600     PERFORM IMS-STATUSKONTROLL                                           
224700     .                                                                    
224800     SKIP3                                                                
225900 IMS-GHU-WDP323 SECTION.                                                  
226000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
226100            DELIMITED BY SIZE INTO SSA1                                   
226200     STRING 'WDP323  (WDP323KY>=' W-WDP323KY-MIN-X                        
226300                    '&WDP323KY=<' W-WDP323KY-MAX-X ')'                    
226400            DELIMITED BY SIZE INTO SSA2                                   
226500     MOVE '  GE' TO GODK-STATUSKODER                                      
226600     CALL CBLTDLI USING GHU WDP3-PCB DLI-IO-WDP323 SSA1 SSA2              
226700     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
226800     PERFORM IMS-STATUSKONTROLL                                           
226900     .                                                                    
227000     SKIP3                                                                
227100 IMS-SOEK-WDP323-PCB2 SECTION.                                            
227200     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
227300            DELIMITED BY SIZE INTO SSA1                                   
227310**   STRING 'WDP323  (IDLAND   =' W-IDLAND-X ')'                          
227320**          DELIMITED BY SIZE INTO SSA2                                   
227400     MOVE 'WDP323 ' TO SSA2                                               
227500     MOVE '  GE' TO GODK-STATUSKODER                                      
227600     CALL CBLTDLI USING GNP WDP32-PCB DLI-IO-WDP323 SSA1 SSA2             
227700     MOVE WDP32-STATUS-CODE TO STATUS-WS                                  
227800     PERFORM IMS-STATUSKONTROLL                                           
227900     .                                                                    
228000     EJECT                                                                
228100 IMS-ISRT-WDP321 SECTION.                                                 
228200     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
228300          DELIMITED BY SIZE INTO SSA1                                     
228400     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
228500          DELIMITED BY SIZE INTO SSA2                                     
228600     MOVE 'WDP321 ' TO SSA3                                               
228700     MOVE '  ' TO GODK-STATUSKODER                                        
228800     CALL CBLTDLI USING ISRT WDP3-PCB DLI-IO-WDP321 SSA1 SSA2 SSA3        
228900     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
229000     PERFORM IMS-STATUSKONTROLL                                           
229100     .                                                                    
229200     SKIP3                                                                
229300 IMS-ISRT-WDP322 SECTION.                                                 
229400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
229500          DELIMITED BY SIZE INTO SSA1                                     
229600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
229700          DELIMITED BY SIZE INTO SSA2                                     
229800     MOVE 'WDP322 ' TO SSA3                                               
229900     MOVE '  ' TO GODK-STATUSKODER                                        
230000     CALL CBLTDLI USING ISRT WDP3-PCB DLI-IO-WDP322 SSA1 SSA2 SSA3        
230100     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
230200     PERFORM IMS-STATUSKONTROLL                                           
230300     .                                                                    
230400     EJECT                                                                
230500 IMS-ISRT-WDP323 SECTION.                                                 
230600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
230700          DELIMITED BY SIZE INTO SSA1                                     
230800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
230900          DELIMITED BY SIZE INTO SSA2                                     
231000     MOVE 'WDP323 ' TO SSA3                                               
231100     MOVE '  ' TO GODK-STATUSKODER                                        
231200     CALL CBLTDLI USING ISRT WDP3-PCB DLI-IO-WDP323 SSA1 SSA2 SSA3        
231300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
231400     PERFORM IMS-STATUSKONTROLL                                           
231500     .                                                                    
231600     EJECT                                                                
231700 IMS-DLET-WDP321 SECTION.                                                 
231800     MOVE '  ' TO GODK-STATUSKODER                                        
231900     CALL CBLTDLI USING DLET WDP3-PCB DLI-IO-WDP321                       
232000     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
232100     PERFORM IMS-STATUSKONTROLL                                           
232200     .                                                                    
232300     SKIP3                                                                
232400 IMS-DLET-WDP322 SECTION.                                                 
232500     MOVE '  ' TO GODK-STATUSKODER                                        
232600     CALL CBLTDLI USING DLET WDP3-PCB DLI-IO-WDP322                       
232700     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
232800     PERFORM IMS-STATUSKONTROLL                                           
232900     .                                                                    
233000     SKIP3                                                                
233100 IMS-DLET-WDP323 SECTION.                                                 
233200     MOVE '  ' TO GODK-STATUSKODER                                        
233300     CALL CBLTDLI USING DLET WDP3-PCB DLI-IO-WDP323                       
233400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
233500     PERFORM IMS-STATUSKONTROLL                                           
233600     .                                                                    
233700     EJECT                                                                
233800 IMS-GU-WDP3A SECTION.                                                    
233900     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
234000                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
234100                    '&KDARBTYP =' W-KDARBTYP ')'                          
234200            DELIMITED BY SIZE INTO SSA1                                   
234300     MOVE '  GE' TO GODK-STATUSKODER                                      
234400     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-WDP3A SSA1                    
234500     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
234600     PERFORM IMS-STATUSKONTROLL                                           
234700     .                                                                    
234800     SKIP2                                                                
234900 IMS-GN-WDP3A SECTION.                                                    
235000     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
235100                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
235200                    '&KDARBTYP =' W-KDARBTYP ')'                          
235300            DELIMITED BY SIZE INTO SSA1                                   
235400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
235500     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-WDP3A SSA1                    
235600     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     SKIP2                                                                
236000 IMS-GU-WDP3B SECTION.                                                    
236100     STRING 'WDP3B1  (WDP3B1KY=>' W-WDP3B1-MIN                            
236110                    '&WDP3B1KY=<' W-WDP3B1-MAX                            
236200                    '&KDARBTYP =' W-KDARBTYP ')'                          
236300            DELIMITED BY SIZE INTO SSA1                                   
236400     MOVE '  GE' TO GODK-STATUSKODER                                      
236500     CALL CBLTDLI USING GU WDP3B-PCB DLI-IO-WDP3B SSA1                    
236600     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
236700     PERFORM IMS-STATUSKONTROLL                                           
236800     .                                                                    
236900     EJECT                                                                
237000 IMS-GN-WDP3B SECTION.                                                    
237100     STRING 'WDP3B1  (WDP3B1KY=>' W-WDP3B1-MIN                            
237110                    '&WDP3B1KY=<' W-WDP3B1-MAX                            
237200                    '&KDARBTYP =' W-KDARBTYP ')'                          
237300            DELIMITED BY SIZE INTO SSA1                                   
237400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
237500     CALL CBLTDLI USING GN WDP3B-PCB DLI-IO-WDP3B SSA1                    
237600     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
237700     PERFORM IMS-STATUSKONTROLL                                           
237800     .                                                                    
237900     SKIP2                                                                
238000 IMS-GU-WDP3C SECTION.                                                    
238100     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
238200                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
238300                    '&KDARBTYP =' W-KDARBTYP ')'                          
238400            DELIMITED BY SIZE INTO SSA1                                   
238500     MOVE '  GE' TO GODK-STATUSKODER                                      
238600     CALL CBLTDLI USING GU WDP3C-PCB DLI-IO-WDP3C SSA1                    
238700     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
238800     PERFORM IMS-STATUSKONTROLL                                           
238900     .                                                                    
239000     SKIP2                                                                
239100 IMS-GN-WDP3C SECTION.                                                    
239200     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
239300                    '&WDP3C1KY=<' W-WDP3C1-MAX                            
239400                    '&KDARBTYP =' W-KDARBTYP ')'                          
239500            DELIMITED BY SIZE INTO SSA1                                   
239600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
239700     CALL CBLTDLI USING GN WDP3C-PCB DLI-IO-WDP3C SSA1                    
239800     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
239900     PERFORM IMS-STATUSKONTROLL                                           
240000     .                                                                    
240100     EJECT                                                                
240200 IMS-STATUSKONTROLL SECTION.                                              
240300                                                                          
240400     SET STATUS-IX TO 1                                                   
240500     SEARCH GODK-STATUS                                                   
240600       AT END                                                             
240700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
240800         DELIMITED BY SIZE INTO FELTEXT                                   
240900         CALL FELLOG                                                      
241000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
241100         CONTINUE                                                         
241200     END-SEARCH                                                           
241300     .                                                                    
