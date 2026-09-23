000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1056100.                                                
000400 AUTHOR.         GUN ANDERSSON.                                           
000500 DATE-WRITTEN.   AUGUSTI 1985.                                            
000510 DATE-COMPILED.                                                           
000600                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        ILLUSTRATIONSREGISTRET - FRÅGA OCH UPPDATERING                   
001100*                                                                         
001200*                                                                         
001300**   INDATA.                                                              
001400**       TRANSAKTION: W1T561                                              
001500*        MID:         W1I56101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W1O56101                                            
001900*                                                                         
002000*    DYNAMISKA SUBPROGRAM:                                                
002100*                     WDATKONV                                            
002200*                     CBLTDLI                                             
002300*                     FELLOG                                              
002400*    EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W1056100'                             
003100                                 PIC X(8).                                
003200 77    JA                        PIC X       VALUE 'J'.                   
003300 77    NEJ                       PIC X       VALUE 'N'.                   
003310 77    OCH                       PIC X       VALUE '&'.                   
003400 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77    SPR-IX                    PIC S9(3)   VALUE +0   COMP SYNC.        
003600 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +893  COMP SYNC.        
003700 77    RADIND                    PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77    KOLIND                    PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77    W-IDCATRAD                PIC S9(3)  VALUE +0    COMP SYNC.        
004000 77    IDRUBNR-FINNS             PIC X       VALUE 'N'.                   
004100 77    INDATA-FEL                PIC X       VALUE 'N'.                   
004200                                                                          
004300 01    FILLER                    PIC X(16)  VALUE 'DYN SUB-PGM'.          
004400 01    DYNAMISKA-SUBPGM.                                                  
004500   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
004600   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
004700   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
004800                                                                          
004900 01    DAGENS-DATUM              PIC 9(6).                                
005000                                                                          
005100 01    DIVERSE.                                                           
005200   03  IDILLU-WS                 PIC X(5).                                
005300   03  IDILLU-WS-N REDEFINES IDILLU-WS   PIC 9(5).                        
005400   03  MID-MARK                  PIC X       VALUE 'N'.                   
005500       88  BORTTAG                           VALUE 'J'.                   
005600                                                                          
005700   03  KOLLBORT                  PIC X       VALUE SPACE.                 
005800   03  IDCATNR-BORT              PIC X       VALUE 'N'.                   
005900   03  IDILLU-BORT               PIC X       VALUE 'N'.                   
006000   03  IDILLU1-BORT              PIC X       VALUE 'N'.                   
006100   03  IDILLU2-BORT              PIC X       VALUE 'N'.                   
006200   03  IDILLU3-BORT              PIC X       VALUE 'N'.                   
006300   03  IDSUBNR-BORT              PIC X       VALUE 'N'.                   
006400                                                                          
006500   03  SPAR-FORDON               PIC X(2)    VALUE SPACE.                 
006600   03  SPAR-IDRUBNR-1            PIC S9(5)     COMP-3.                    
006700   03  SPAR-IDRUBNR-2            PIC S9(5)     COMP-3.                    
006800   03  SPAR-IDRUBNR-3            PIC S9(5)     COMP-3.                    
006900   03  SPARA-PFK                 PIC X       VALUE SPACE.                 
007000   03  UPPDATKOD                 PIC X       VALUE SPACE.                 
007100   03  IN-FLRUBTYP               PIC X       VALUE SPACE.                 
007200   03  BAS-IDRUBNR  OCCURS 3     PIC S9(5).                               
007300   03  TEST-IDRUBNR OCCURS 5     PIC S9(5).                               
007400   03  BAS-FLRUBTYP              PIC X       VALUE SPACE.                 
007500   03  TEST-FLRUBTYP             PIC X       VALUE SPACE.                 
007600   03  DEL-MAPP                  PIC 9(3).                                
007700   03  DEL-M-X  REDEFINES DEL-MAPP.                                       
007800     05 DEL-FORD                 PIC 9(2).                                
007900     05 DEL-SUB                  PIC 9.                                   
008000                                                                          
009300   03  RED-CATALOG.                                                       
009400     05  RED-CATNR               PIC ZZZZ9   VALUE ZERO.                  
009500     05  FILLER                  PIC X       VALUE SPACE.                 
009600     05  RED-CATGRP              PIC Z9      VALUE ZERO.                  
009700     05  FILLER                  PIC X       VALUE '-'.                   
009800     05  RED-CATAVS              PIC ZZZ9    VALUE ZERO.                  
009810     05  FILLER                  PIC X       VALUE SPACE.                 
009820     05  RED-KDCATPUB-R          PIC X(3)    VALUE '---'.                 
009900                                                                          
010000   03    DEL-RUBNR-N             PIC 9(5).                                
010100   03    DEL-RUBNR REDEFINES  DEL-RUBNR-N.                                
010200     05  DEL-RUB12               PIC 9(2).                                
010300     05  FILLER                  PIC 9(3).                                
010400   03    IN-IDRUBNR  OCCURS 5    PIC S9(5)               COMP-3.          
010500                                                                          
010600   03  TEST-ILLU.                                                         
010700     05  FILLER                  PIC S9(5)               COMP-3.          
010800     05  TEST-ILLU-IDCATNR       PIC 9(5)          VALUE ZERO.            
010900     05  TEST-ILLU-IDILLU1       PIC S9(5)  COMP-3  VALUE ZERO.           
011000     05  TEST-ILLU-IDILLU2       PIC S9(5)  COMP-3  VALUE ZERO.           
011100     05  TEST-ILLU-IDILLU3       PIC S9(5)  COMP-3  VALUE ZERO.           
011200     05  FILLER                  PIC X(2).                                
011300     05  TEST-ILLU-IDMAPP        PIC S9(3)  COMP-3  VALUE ZERO.           
011400     05  TEST-ILLU-IDRUBNR1      PIC S9(5)  COMP-3  VALUE ZERO.           
011500     05  TEST-ILLU-IDRUBNR2      PIC S9(5)  COMP-3  VALUE ZERO.           
011600     05  TEST-ILLU-IDRUBNR3      PIC S9(5)  COMP-3  VALUE ZERO.           
011700     05  FILLER                  PIC X.                                   
011800     05  FILLER                  PIC S9(7)  COMP-3  VALUE ZERO.           
011900     05  FILLER                  PIC S9(7)  COMP-3  VALUE ZERO.           
012000     05  TEST-ILLU-TENOTE        PIC X(40).                               
012100                                                                          
012200   03  TEST-DEL-SUBMAPP          PIC 9(3).                                
012300   03  TEST-SUBMAPP  REDEFINES TEST-DEL-SUBMAPP.                          
012400     05  FILLER                  PIC 9(2).                                
012500     05  TEST-ILLU-SUBMAPP       PIC 9.                                   
012600     EJECT                                                                
012700 01    NYCKLAR-TILL-DLI.                                                  
012800   03    W-IDILLU-X.                                                      
012900     05    W-IDILLU              PIC S9(5)   VALUE ZERO  COMP-3.          
012930                                                                          
013000   03    W-IDCATNR-X.                                                     
013100     05    W-IDCATNR             PIC 9(5)    VALUE ZERO.                  
013110                                                                          
013200   03    W-IDRUBNR-X.                                                     
013300     05    W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
013310                                                                          
013400   03    W-IDSKYLT               PIC X(3).                                
013410                                                                          
013500   03    W-1209-KEY-X.                                                    
013600     05  FILLER                  PIC X(4)    VALUE '1209'.                
013700     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
013701*    --- UNIK LÄSNING                                                     
013710   03  W-WDN5C1KY-X.                                                      
013720       05 W-IDILLU-AVSC-X .                                               
013730         07 W-IDILLU-AVSC        PIC S9(5)  VALUE ZERO COMP-3.            
013740                                                                          
013780       05 W-KDCATPUB-AVSC        PIC X(6)   VALUE LOW-VALUE.              
013781                                                                          
013784       05 W-IDKATAKY-AVSC.                                                
013785         07 W-IDCATNR-AVSC       PIC 9(5)   VALUE ZERO.                   
013786         07 W-IDCATGRP-AVSC      PIC 9(2)   VALUE ZERO.                   
013787         07 W-IDCATAVS-AVSC      PIC 9(4)   VALUE ZERO.                   
013788                                                                          
013789*   --- MIN-NYCKEL PÅ WDN5C1                                              
013790   03  W-WDN5C1KY-MIN-X.                                                  
013791       05 W-IDILLU-AVSC-MIN-X .                                           
013792         07 W-IDILLU-AVSC-MIN    PIC S9(5)  VALUE ZERO COMP-3.            
013793                                                                          
013794       05 W-KDCATPUB-AVSC-MIN    PIC X(6)   VALUE LOW-VALUE.              
013795                                                                          
013796       05 W-IDKATAKY-AVSC-MIN.                                            
013797         07 W-IDCATNR-AVSC-MIN   PIC 9(5)   VALUE ZERO.                   
013798         07 W-IDCATGRP-AVSC-MIN  PIC 9(2)   VALUE ZERO.                   
013799         07 W-IDCATAVS-AVSC-MIN  PIC 9(4)   VALUE ZERO.                   
013800                                                                          
013801*   --- MAX-NYCKEL PÅ WDN5C1                                              
013802   03  W-WDN5C1KY-MAX-X.                                                  
013803       05 W-IDILLU-AVSC-MAX-X .                                           
013804         07 W-IDILLU-AVSC-MAX    PIC S9(5)  VALUE ZERO COMP-3.            
013805                                                                          
013806       05 W-KDCATPUB-AVSC-MAX    PIC X(6)   VALUE HIGH-VALUE.             
013807                                                                          
013808       05 W-IDKATAKY-AVSC-MAX.                                            
013809         07 W-IDCATNR-AVSC-MAX   PIC 9(5)   VALUE 99999.                  
013810         07 W-IDCATGRP-AVSC-MAX  PIC 9(2)   VALUE 99.                     
013811         07 W-IDCATAVS-AVSC-MAX  PIC 9(4)   VALUE 9999.                   
013812                                                                          
013820     SKIP3                                                                
013900 01    MEDDELANDE.                                                        
014000   03    FEL-01.                                                          
014100      05 FILLER                  PIC X(15)   VALUE                        
014200                'NYCKEL FELAKTIG'.                                        
014300      05 FILLER                  PIC X(15)   VALUE                        
014400                'WRONG KEY      '.                                        
014500   03 FILLER REDEFINES FEL-01.                                            
014600      05 FEL-1 OCCURS 2  PIC X(15).                                       
014700*                                                                         
014800   03    FEL-02.                                                          
014900      05 FILLER                  PIC X(22)   VALUE                        
015000                'ILLUSTRATIONEN SAKNAS '.                                 
015100      05 FILLER                  PIC X(22)   VALUE                        
015200                'ILLUSTRATION NOT FOUND'.                                 
015300   03 FILLER REDEFINES FEL-02.                                            
015400      05 FEL-2 OCCURS 2  PIC X(22).                                       
015500*                                                                         
015600   03    FEL-03.                                                          
015700      05 FILLER                  PIC X(32)   VALUE                        
015800             'ILLUSTRATIONEN FINNS REDAN      '.                          
015900      05 FILLER                  PIC X(32)   VALUE                        
016000             'ILLUSTRATION ALREADY REGISTRATED'.                          
016100   03 FILLER REDEFINES FEL-03.                                            
016200      05 FEL-3 OCCURS 2  PIC X(32).                                       
016300*                                                                         
016400   03    FEL-04.                                                          
016500      05 FILLER                  PIC X(19)   VALUE                        
016600             'MARKERADE FÄLT FEL '.                                       
016700      05 FILLER                  PIC X(19)   VALUE                        
016800             'MARKED FIELDS WRONG'.                                       
016900   03 FILLER REDEFINES FEL-04.                                            
017000      05 FEL-4 OCCURS 2  PIC X(19).                                       
017100*                                                                         
017200   03    FEL-05.                                                          
017300      05 FILLER                  PIC X(35)   VALUE                        
017400             'ILLUSTRATIONEN KAN EJ TAS BORT     '.                       
017500      05 FILLER                  PIC X(35)   VALUE                        
017600             'THE ILLUSTRATION CAN NOT BE DELETED'.                       
017700   03 FILLER REDEFINES FEL-05.                                            
017800      05 FEL-5 OCCURS 2  PIC X(35).                                       
017900*                                                                         
018000   03    FEL-06.                                                          
018100      05 FILLER                  PIC X(31)   VALUE                        
018200             'INGEN UPPDATERING GJORD        '.                           
018300      05 FILLER                  PIC X(31)   VALUE                        
018400             'DATABASE HAS  NOT  BEEN UPDATED'.                           
018500   03 FILLER REDEFINES FEL-06.                                            
018600      05 FEL-6 OCCURS 2  PIC X(31).                                       
018700*                                                                         
018800   03    FEL-07.                                                          
018900      05 FILLER                 PIC X(19)   VALUE                         
019000             'INGET ATT UPPDATERA'.                                       
019100      05 FILLER                 PIC X(19)   VALUE                         
019200             'NOTHING TO UPDATE  '.                                       
019300   03 FILLER REDEFINES FEL-07.                                            
019400      05 FEL-7 OCCURS 2  PIC X(19).                                       
019500*                                                                         
019600   03    FEL-08.                                                          
019700      05 FILLER                 PIC X(27)   VALUE                         
019800             'ILLUSTRATIONSNR EJ TILLÅTET'.                               
019900      05 FILLER                 PIC X(27)   VALUE                         
020000             'ILLUSTRATION-NO NOT ALLOWED'.                               
020100   03 FILLER REDEFINES FEL-08.                                            
020200      05 FEL-8 OCCURS 2  PIC X(27).                                       
020300*                                                                         
020400   03    FEL-09.                                                          
020500      05 FILLER                  PIC X(28)   VALUE                        
020600             'FRÅGA FÖRE UPPDATERING      '.                              
020700      05 FILLER                  PIC X(28)   VALUE                        
020800             'MAKE A QUERY BEFORE UPDATING'.                              
020900   03 FILLER REDEFINES FEL-09.                                            
021000      05 FEL-9 OCCURS 2  PIC X(28).                                       
021100*                                                                         
021200   03    FEL-010.                                                         
021300      05 FILLER                  PIC X(33)   VALUE                        
021400             'ILLUSTRATIONSNR.INTERVALLET FULLT'.                         
021500      05 FILLER                  PIC X(33)   VALUE                        
021600             'ILLUSTRATION-NO. INTERVAL IS FULL'.                         
021700   03 FILLER REDEFINES FEL-010.                                           
021800      05 FEL-10 OCCURS 2 PIC X(33).                                       
021900*                                                                         
022000   03    MTEXT01.                                                         
022100      05 FILLER                  PIC X(19)   VALUE                        
022200             'UPPDATERING GJORD  '.                                       
022300      05 FILLER                  PIC X(19)   VALUE                        
022400             'DATABASE IS UPDATED'.                                       
022500   03 FILLER REDEFINES MTEXT01.                                           
022600      05 MTEXT1 OCCURS 2 PIC X(19).                                       
022700*                                                                         
022800   03    MTEXT02.                                                         
022900      05 FILLER                  PIC X(24)   VALUE                        
023000             'ILLUSTRATIONEN ÄNDRAD   '.                                  
023100      05 FILLER                  PIC X(24)   VALUE                        
023200             'ILLUSTRATION IS REPLACED'.                                  
023300   03 FILLER REDEFINES MTEXT02.                                           
023400      05 MTEXT2 OCCURS 2 PIC X(24).                                       
023500*                                                                         
023600   03    MTEXT03.                                                         
023700      05 FILLER                  PIC X(24)   VALUE                        
023800             'ILLUSTRATIONEN TILLAGD  '.                                  
023900      05 FILLER                  PIC X(24)   VALUE                        
024000             'ILLUSTRATION IS INSERTED'.                                  
024100   03 FILLER REDEFINES MTEXT03.                                           
024200      05 MTEXT3 OCCURS 2 PIC X(24).                                       
024300*                                                                         
024400   03    MTEXT04.                                                         
024500      05 FILLER                  PIC X(33)   VALUE                        
024600             'RUBRIKNR SAKNAS I RUBRIKLEXIKON  '.                         
024700      05 FILLER                  PIC X(33)   VALUE                        
024800             'HEADING-NO NOT IN HEADING-LEXICON'.                         
024900   03 FILLER REDEFINES MTEXT04.                                           
025000      05 MTEXT4 OCCURS 2 PIC X(33).                                       
025100*                                                                         
025200     EJECT                                                                
025300******************************************************************        
025400*                AREA FÖR DATUMKKONTROLL                                  
025500*                                                                         
025600 01    FILLER                    PIC X(16)   VALUE 'WDATAREA'.            
025700     SKIP3                                                                
025800*01   -COPY WDATAREA.                                                     
025900*++INCLUDE WDATAREAC0                                                     
026000     EJECT                                                                
026100******************************************************************        
026200*                                                                         
026300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
026400*                                                                         
026500 01    FILLER                    PIC X(16)   VALUE 'MID-AREA'.            
026600     SKIP3                                                                
026700*01    MID -COPY W1I56101.                                                
026800*++INCLUDE W1I56101C0                                                     
026900     EJECT                                                                
027000*01    -COPY WMSGAREA                                                     
027100*++INCLUDE WMSGAREAC0                                                     
027200     EJECT                                                                
027300*  03    MOD -COPY W1O56101 -RED MSG-AREA.                                
027400*++INCLUDE W1O56101C0                                                     
027500     EJECT                                                                
027600*01    -COPY WMFSAREA                                                     
027700*++INCLUDE WMFSAREAC0                                                     
027800     EJECT                                                                
027900******************************************************************        
028000*                                                                         
028100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028200*                                                                         
028300 01    IMS-WS.                                                            
028400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
028500     SKIP3                                                                
028600*                        **** STATUS-KOD FRÅN IMS                         
028700   03    STATUS-WS               PIC XX.                                  
028800     88    SEGMENT-FINNS                     VALUE '  '.                  
028900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
029000     SKIP3                                                                
029100   03    GODK-STATUSKODER.                                                
029200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
029300     SKIP3                                                                
029400 01    SSA1                      PIC X(128).                              
029500 01    SSA2                      PIC X(128).                              
029600     EJECT                                                                
029700*                            IMS FUNKTIONSKODER                           
029800*01    -COPY W0003                                                        
030000     EJECT                                                                
030100*                            DLI INPUT-OUTPUT AREA                        
030200 01    DLI-IO-AREA.                                                       
030300   03    IO-AREA                 PIC X(80)  VALUE SPACE.                  
030400     SKIP3                                                                
030500*  03    WLKATK01 -COPY WDN701  -RED IO-AREA.                             
030700     EJECT                                                                
031100   03    IO-AREA2               PIC X(500).                               
031200*  03    WLKATM01 -COPY WDN101  -RED IO-AREA2.                            
031400     EJECT                                                                
031500*  03    WLKATB01 -COPY WDN201   -RED IO-AREA2.                           
031700     EJECT                                                                
031800*  03    WLKATB11 -COPY WDN211   -RED IO-AREA2.                           
032000*  03    WLXXAJ11 -COPY WDGX1210 -RED IO-AREA2.                           
032200     EJECT                                                                
032210   03    IO-AREA3               PIC X(24).                                
032220*  03    WLKATO01 -COPY WDN5C1   -RED IO-AREA3.                           
032230     EJECT                                                                
032300 LINKAGE SECTION.                                                         
032400*01    -COPY W0009     -PRE MSG-                                          
032600     EJECT                                                                
032700*01    -COPY W0008     -PRE ILLU-                                         
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01    -COPY W0008     -PRE KAT-                                          
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01    -COPY W0008     -PRE RUB-                                          
033700     05  FILLER                  PIC X.                                   
033800*01    -COPY W0008     -PRE HTR-                                          
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034110*01    -COPY W0008     -PRE KATO-                                         
034120     05  FILLER                  PIC X.                                   
034130     EJECT                                                                
034200 PROCEDURE DIVISION USING MSG-PCB ILLU-PCB KAT-PCB RUB-PCB                
034300                          HTR-PCB KATO-PCB.                               
034400     ENTRY 'DLITCBL' USING MSG-PCB ILLU-PCB KAT-PCB RUB-PCB               
034500                           HTR-PCB KATO-PCB.                              
034600     PERFORM IMS-GET-MSG                                                  
034700     IF SEGMENT-FINNS                                                     
034800       PERFORM A-INIT-SPARA-INPUT                                         
034900       IF IDILLU-WS NOT NUMERIC                                           
035000          MOVE FEL-1(SPR-IX) TO MOD-TEMFSFEL                              
035100          PERFORM I-RENSA-BILD                                            
035200          PERFORM J-GRUND-FORMAT                                          
035300       ELSE                                                               
035400          IF MFS-UPDATE                                                   
035500             PERFORM B-KONTROLLERA-NYCKEL                                 
035600             IF INDATA-FEL = JA                                           
035700               IF MID-IDILLU-IN NOT = ALL '+'                             
035800                  PERFORM H-VISA-BILD-IGEN                                
035900               END-IF                                                     
036000             ELSE                                                         
036100                PERFORM C-KONTROLLERA-FAELT                               
036200                IF INDATA-FEL = JA                                        
036300                   MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL                     
036400                   PERFORM H-VISA-BILD-IGEN                               
036500                ELSE                                                      
036600                   IF UPPDATKOD = 'B'                                     
036700                      PERFORM E-BORTTAG                                   
036800                      IF INDATA-FEL = JA                                  
036900                         MOVE FEL-5(SPR-IX) TO MOD-TEMFSFEL               
037000                      END-IF                                              
037100                   ELSE                                                   
037200                     IF UPPDATKOD = 'A'                                   
037300                        IF IDRUBNR-FINNS = JA                             
037400                          PERFORM S01-KOLLA-RUB-MED-BAS                   
037500                        END-IF                                            
037600                        IF INDATA-FEL = NEJ                               
037700                          PERFORM F-AENDRING                              
037800                          IF INDATA-FEL = JA                              
037900                            MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL            
038000                          END-IF                                          
038100                        ELSE                                              
038200                          MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL              
038300                        END-IF                                            
038400                     ELSE                                                 
038500                        IF UPPDATKOD = 'N'                                
038600                           PERFORM G-NYUPPL                               
038700                        END-IF                                            
038800                     END-IF                                               
038900                   END-IF                                                 
039000                   IF INDATA-FEL = JA                                     
039100                      PERFORM H-VISA-BILD-IGEN                            
039200                   ELSE                                                   
039300                      IF UPPDATKOD = 'B'                                  
039400                         PERFORM I-RENSA-BILD                             
039500                      ELSE                                                
039600                         PERFORM D-FRAGEBILD                              
039700                         MOVE MTEXT1(SPR-IX) TO MOD-TEMFSINF              
039800                      END-IF                                              
039900                      PERFORM J-GRUND-FORMAT                              
040000                   END-IF                                                 
040100                END-IF                                                    
040200             END-IF                                                       
040300          ELSE                                                            
040310             IF MFS-FIRST                                                 
040400                PERFORM D-FRAGEBILD                                       
040500                PERFORM J-GRUND-FORMAT                                    
040600                IF INDATA-FEL = JA                                        
040700                   MOVE FEL-2(SPR-IX) TO MOD-TEMFSFEL                     
040800                END-IF                                                    
040810             ELSE                                                         
040820                IF MFS-NEXT                                               
040830                   PERFORM K-NAESTA-SIDA                                  
040880                ELSE                                                      
040890                   PERFORM H-VISA-BILD-IGEN                               
040900                END-IF                                                    
040910             END-IF                                                       
040920          END-IF                                                          
041000       END-IF                                                             
041100     END-IF                                                               
041200     MOVE LENGTH OF MOD  TO MSG-KVLL                                      
041210     ADD +4              TO MSG-KVLL                                      
041300     PERFORM IMS-INSERT-MSG                                               
041400     MOVE ZERO TO RETURN-CODE                                             
041500     GOBACK                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 A-INIT-SPARA-INPUT SECTION.                                              
041900     IF MSG-DUBBLA-TRANSKODER                                             
042000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I56101                 
042100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
042200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
042500     ELSE                                                                 
042600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I56101                  
042700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
042800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
043000     END-IF                                                               
043010     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
043020     MOVE MSG-IDPFK            TO MFS-IDPFK                               
043100     IF MFS-IDTRANS NOT = '1561'                                          
043200       MOVE SPACE TO MFS-KDTRTYP                                          
043300       MOVE '7'   TO MFS-IDPFK                                            
043400     END-IF                                                               
043500                                                                          
043600     IF MID-IDILLU-IN = ALL '+'                                           
043700       MOVE MID-IDILLU-UT TO IDILLU-WS                                    
043800       INSPECT IDILLU-WS REPLACING ALL SPACE BY ZERO                      
043900     ELSE                                                                 
044000       MOVE MID-IDILLU-IN TO IDILLU-WS                                    
044001       MOVE SPACE TO MFS-KDTRTYP                                          
044010       MOVE '7'   TO MFS-IDPFK                                            
044100     END-IF                                                               
044200     MOVE LOW-VALUE TO MSG-AREA                                           
044300     MOVE 'W1O56101' TO MFS-IDMOD                                         
044400     MOVE '1561' TO MOD-IDTRANS                                           
044500                                                                          
044600     IF ENGLISH-TEXT                                                      
044700        MOVE +2 TO SPR-IX                                                 
044800     ELSE                                                                 
044900        MOVE +1 TO SPR-IX                                                 
045000     END-IF                                                               
045100                                                                          
045200     MOVE IDILLU-WS TO MOD-IDILLU-UT                                      
045300     INSPECT MOD-IDILLU-UT REPLACING LEADING ZERO BY SPACE                
045400     MOVE MFS-RENSA-FAELT TO MOD-IDILLU-IN                                
045500                             MOD-TEMFSFEL                                 
045600                             MOD-TEMFSINF                                 
045700     ACCEPT DAGENS-DATUM FROM DATE                                        
045800     EJECT                                                                
045900     .                                                                    
046000 B-KONTROLLERA-NYCKEL  SECTION.                                           
046100     SKIP2                                                                
046200     IF MID-IDILLU-IN = ALL '+'                                           
046300        IF MID-IDCATNR = ALL '+'  AND                                     
046400           MID-IDILLU-1 = ALL '+'  AND                                    
046500           MID-IDILLU-2 = ALL '+'  AND                                    
046600           MID-IDILLU-3 = ALL '+'  AND                                    
046700           MID-IDSUBNR  = ALL '+'  AND                                    
046800           MID-IDRUBNR (1) = ALL '+'  AND                                 
046900           MID-IDRUBNR (2) = ALL '+'  AND                                 
047000           MID-IDRUBNR (3) = ALL '+'  AND                                 
047100           MID-IDRUBNR (4) = ALL '+'  AND                                 
047200           MID-IDRUBNR (5) = ALL '+'  AND                                 
047300           MID-TENOTE  = ALL '+'                                          
047400              MOVE JA TO INDATA-FEL                                       
047500              MOVE FEL-7(SPR-IX) TO MOD-TEMFSFEL                          
047600        ELSE                                                              
047700           MOVE IDILLU-WS-N   TO W-IDILLU                                 
047800           IF W-IDILLU = ZERO                                             
047900              MOVE 'N' TO UPPDATKOD                                       
048000           ELSE                                                           
048100              PERFORM IMS-GET-ILLU                                        
048200              IF SEGMENT-FINNS                                            
048300                 MOVE ILLU-WDN701  TO TEST-ILLU                           
048400                 MOVE TEST-ILLU-IDMAPP  TO TEST-DEL-SUBMAPP               
048500                 MOVE 'A' TO UPPDATKOD                                    
048600              ELSE                                                        
048700                 IF IDILLU-WS-N > ZERO AND IDILLU-WS-N < 25000            
048710***                 * UTÖKAT FÖR 'NL'                                     
048800                    MOVE 'N' TO UPPDATKOD                                 
048900                 ELSE                                                     
049000                    MOVE JA TO INDATA-FEL                                 
049100                    PERFORM I-RENSA-BILD                                  
049200                    MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                    
049300                 END-IF                                                   
049400              END-IF                                                      
049500           END-IF                                                         
049600        END-IF                                                            
049700     ELSE                                                                 
049800        MOVE JA TO INDATA-FEL                                             
049900        PERFORM I-RENSA-BILD                                              
050000        MOVE FEL-9(SPR-IX) TO MOD-TEMFSFEL                                
050100     END-IF                                                               
050200     EJECT                                                                
050300     .                                                                    
050400 C-KONTROLLERA-FAELT  SECTION.                                            
050500                                                                          
050600     PERFORM CA-KONTR-CATNR                                               
050700     PERFORM CB-KONTR-FRAN                                                
050800     PERFORM CC-KONTR-SUBMAPP                                             
050900     PERFORM CD-KONTR-RUBNR                                               
051000                                                                          
051100     MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR                         
051200     IF MID-TENOTE NOT = ALL '+'                                          
051300        MOVE MID-TENOTE TO TEST-ILLU-TENOTE                               
051400     END-IF                                                               
051500                                                                          
051600                                                                          
051700     IF INDATA-FEL = NEJ                                                  
051800        IF IDRUBNR-FINNS = JA                                             
051900           PERFORM CE-LAS-BAS-RUBNR                                       
052000        END-IF                                                            
052100        IF INDATA-FEL = NEJ                                               
052200           IF IDRUBNR-FINNS = JA                                          
052300              PERFORM S01-KOLLA-RUB-MED-BAS                               
052400           END-IF                                                         
052500           IF INDATA-FEL = NEJ                                            
052600              IF TEST-ILLU-IDCATNR = ZERO  AND                            
052700                 TEST-ILLU-IDILLU1 = ZERO  AND                            
052800                 TEST-ILLU-IDILLU2 = ZERO  AND                            
052900                 TEST-ILLU-IDILLU3 = ZERO  AND                            
053000                 TEST-ILLU-IDRUBNR1 = ZERO  AND                           
053100                 TEST-ILLU-IDRUBNR2 = ZERO  AND                           
053200                 TEST-ILLU-IDRUBNR3 = ZERO  AND                           
053300                 TEST-ILLU-TENOTE = SPACE  AND                            
053400                 TEST-ILLU-SUBMAPP = ZERO                                 
053500                 MOVE 'B' TO UPPDATKOD                                    
053600              END-IF                                                      
053700           END-IF                                                         
053800        END-IF                                                            
053900     END-IF                                                               
054000     EJECT                                                                
054100     .                                                                    
054200 CA-KONTR-CATNR  SECTION.                                                 
054300     SKIP3                                                                
054400     IF MID-IDCATNR = ALL '+'                                             
054500        CONTINUE                                                          
054600     ELSE                                                                 
054700       IF MID-IDCATNR NOT NUMERIC                                         
054800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-ATTR                       
054900         MOVE JA TO INDATA-FEL                                            
055000       ELSE                                                               
055100         MOVE MID-IDCATNR TO TEST-ILLU-IDCATNR                            
055200         IF MID-IDCATNR NOT = ZERO                                        
055300           MOVE MID-IDCATNR TO W-IDCATNR                                  
055400           PERFORM IMS-GET-KAT                                            
055500           IF SEGMENT-FINNS                                               
055600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-ATTR                 
055700             MOVE KAT-KDFORDON TO SPAR-FORDON                             
055800           ELSE                                                           
055900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-ATTR                   
056000             MOVE JA TO INDATA-FEL                                        
056100           END-IF                                                         
056200         END-IF                                                           
056300       END-IF                                                             
056400     END-IF                                                               
056500     EJECT                                                                
056600     .                                                                    
056700 CB-KONTR-FRAN  SECTION.                                                  
056800                                                                          
056900       IF MID-IDILLU-1 = ALL '+'                                          
057000          CONTINUE                                                        
057100       ELSE                                                               
057200         IF MID-IDILLU-1 NOT NUMERIC                                      
057300           MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-1-ATTR                    
057400           MOVE JA TO INDATA-FEL                                          
057500         ELSE                                                             
057600           MOVE MID-IDILLU-1 TO TEST-ILLU-IDILLU1                         
057700           IF MID-IDILLU-1 = ZERO                                         
057800             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-1-ATTR                
057900           ELSE                                                           
058000             MOVE MID-IDILLU-1 TO W-IDILLU                                
058100             PERFORM IMS-GET-ILLU                                         
058200             IF SEGMENT-SAKNAS                                            
058300               MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-1-ATTR                
058400               MOVE JA TO INDATA-FEL                                      
058500             ELSE                                                         
058600               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-1-ATTR              
058700             END-IF                                                       
058800           END-IF                                                         
058900         END-IF                                                           
059000       END-IF                                                             
059100                                                                          
059200       IF MID-IDILLU-2 = ALL '+'                                          
059300          CONTINUE                                                        
059400       ELSE                                                               
059500         IF MID-IDILLU-2 NOT NUMERIC                                      
059600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-2-ATTR                    
059700           MOVE JA TO INDATA-FEL                                          
059800         ELSE                                                             
059900           MOVE MID-IDILLU-2 TO TEST-ILLU-IDILLU2                         
060000           IF MID-IDILLU-2 = ZERO                                         
060100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-2-ATTR                
060200           ELSE                                                           
060300             MOVE MID-IDILLU-2 TO W-IDILLU                                
060400             PERFORM IMS-GET-ILLU                                         
060500             IF SEGMENT-SAKNAS                                            
060600               MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-2-ATTR                
060700               MOVE JA TO INDATA-FEL                                      
060800             ELSE                                                         
060900               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-2-ATTR              
061000             END-IF                                                       
061100           END-IF                                                         
061200         END-IF                                                           
061300       END-IF                                                             
061400                                                                          
061500       IF MID-IDILLU-3 = ALL '+'                                          
061600          CONTINUE                                                        
061700       ELSE                                                               
061800         IF MID-IDILLU-3 NOT NUMERIC                                      
061900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-3-ATTR                    
062000           MOVE JA TO INDATA-FEL                                          
062100         ELSE                                                             
062200           MOVE MID-IDILLU-3 TO TEST-ILLU-IDILLU3                         
062300           IF MID-IDILLU-3 = ZERO                                         
062400             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-3-ATTR                
062500           ELSE                                                           
062600             MOVE MID-IDILLU-3 TO W-IDILLU                                
062700             PERFORM IMS-GET-ILLU                                         
062800             IF SEGMENT-SAKNAS                                            
062900               MOVE MFS-NUM-FAELT-FEL TO MOD-IDILLU-3-ATTR                
063000               MOVE JA TO INDATA-FEL                                      
063100             ELSE                                                         
063200               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDILLU-3-ATTR              
063300             END-IF                                                       
063400           END-IF                                                         
063500         END-IF                                                           
063600       END-IF                                                             
063700                                                                          
063800     EJECT                                                                
063900     .                                                                    
064000 CC-KONTR-SUBMAPP  SECTION.                                               
064100                                                                          
064200       IF MID-IDSUBNR = ALL '+'                                           
064300          CONTINUE                                                        
064400       ELSE                                                               
064500         IF MID-IDSUBNR NOT NUMERIC                                       
064600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDSUBNR-ATTR                     
064700           MOVE JA TO INDATA-FEL                                          
064800         ELSE                                                             
064900           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDSUBNR-ATTR                   
065000           MOVE MID-IDSUBNR  TO TEST-ILLU-SUBMAPP                         
065100         END-IF                                                           
065200       END-IF                                                             
065300     EJECT                                                                
065400     .                                                                    
065500 CD-KONTR-RUBNR  SECTION.                                                 
065600                                                                          
065700     MOVE 'J'  TO IN-FLRUBTYP                                             
065800     MOVE +1 TO INDX                                                      
065900     PERFORM UNTIL INDX NOT < 6                                           
066000        IF MID-IDRUBNR(INDX) NOT = ALL '+'                                
066100           IF MID-IDRUBNR(INDX) NUMERIC                                   
066200              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRUBNR-ATTR(INDX)          
066300              MOVE  JA  TO IDRUBNR-FINNS                                  
066400              MOVE MID-IDRUBNR (INDX) TO IN-IDRUBNR (INDX)                
066500           ELSE                                                           
066600              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(INDX)            
066700              MOVE  JA  TO INDATA-FEL                                     
066800           END-IF                                                         
066900        ELSE                                                              
067000           MOVE ZERO TO IN-IDRUBNR (INDX)                                 
067100        END-IF                                                            
067200        ADD +1 TO INDX                                                    
067300     END-PERFORM                                                          
067400                                                                          
067500     IF INDATA-FEL =  NEJ  AND IDRUBNR-FINNS =  JA                        
067600        IF (IN-IDRUBNR(2) NOT = ZERO) OR                                  
067700           (IN-IDRUBNR(3) NOT = ZERO)                                     
067800           IF (IN-IDRUBNR(4) = ZERO)                                      
067900              CONTINUE                                                    
068000           ELSE                                                           
068100              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)               
068200              MOVE  JA  TO INDATA-FEL                                     
068300           END-IF                                                         
068400           IF (IN-IDRUBNR(5) = ZERO)                                      
068500              CONTINUE                                                    
068600           ELSE                                                           
068700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)               
068800              MOVE  JA  TO INDATA-FEL                                     
068900           END-IF                                                         
069000        END-IF                                                            
069100                                                                          
069200        IF INDATA-FEL =  NEJ                                              
069300           IF (IN-IDRUBNR(4) NOT = ZERO) OR                               
069400              (IN-IDRUBNR(5) NOT = ZERO)                                  
069500              MOVE 'N' TO IN-FLRUBTYP                                     
069600           END-IF                                                         
069700        END-IF                                                            
069800     END-IF                                                               
069900     EJECT                                                                
070000     .                                                                    
070100 CE-LAS-BAS-RUBNR SECTION.                                                
070200     SKIP2                                                                
070300     MOVE ZERO TO BAS-IDRUBNR(1)                                          
070400                  BAS-IDRUBNR(2)                                          
070500                  BAS-IDRUBNR(3)                                          
070600     MOVE IN-FLRUBTYP TO BAS-FLRUBTYP                                     
070700                                                                          
070800     MOVE ZERO TO TEST-IDRUBNR(1)                                         
070900                  TEST-IDRUBNR(2)                                         
071000                  TEST-IDRUBNR(3)                                         
071100                  TEST-IDRUBNR(4)                                         
071200                  TEST-IDRUBNR(5)                                         
071300                                                                          
071400     IF UPPDATKOD = 'A'                                                   
071500       MOVE ILLU-IDRUBNR-1  TO BAS-IDRUBNR (1)                            
071600       MOVE ILLU-IDRUBNR-2  TO BAS-IDRUBNR (2)                            
071700       MOVE ILLU-IDRUBNR-3  TO BAS-IDRUBNR (3)                            
071800       MOVE ILLU-FLRUBTYP   TO BAS-FLRUBTYP                               
071900     END-IF                                                               
072000                                                                          
072100     MOVE BAS-IDRUBNR(1) TO TEST-IDRUBNR(1)                               
072200     IF BAS-FLRUBTYP = 'J'                                                
072300        MOVE BAS-IDRUBNR(2) TO TEST-IDRUBNR(2)                            
072400        MOVE BAS-IDRUBNR(3) TO TEST-IDRUBNR(3)                            
072500     ELSE                                                                 
072600        MOVE BAS-IDRUBNR(2) TO TEST-IDRUBNR(4)                            
072700        MOVE BAS-IDRUBNR(3) TO TEST-IDRUBNR(5)                            
072800     END-IF                                                               
072900                                                                          
073000     IF MID-IDRUBNR(1) NOT = ALL '+'                                      
073100        MOVE MID-IDRUBNR(1) TO TEST-IDRUBNR(1)                            
073200     END-IF                                                               
073300                                                                          
073400     IF MID-IDRUBNR(2) NOT = ALL '+'                                      
073500        MOVE MID-IDRUBNR(2) TO TEST-IDRUBNR(2)                            
073600     END-IF                                                               
073700                                                                          
073800     IF MID-IDRUBNR(3) NOT = ALL '+'                                      
073900        MOVE MID-IDRUBNR(3) TO TEST-IDRUBNR(3)                            
074000     END-IF                                                               
074100                                                                          
074200     IF MID-IDRUBNR(4) NOT = ALL '+'                                      
074300        MOVE MID-IDRUBNR(4) TO TEST-IDRUBNR(4)                            
074400     END-IF                                                               
074500                                                                          
074600     IF MID-IDRUBNR(5) NOT = ALL '+'                                      
074700        MOVE MID-IDRUBNR(5) TO TEST-IDRUBNR(5)                            
074800     END-IF                                                               
074900                                                                          
075000     IF TEST-IDRUBNR(2) NOT = ZERO OR                                     
075100        TEST-IDRUBNR(3) NOT = ZERO                                        
075200        IF TEST-IDRUBNR(4) NOT = ZERO OR                                  
075300           TEST-IDRUBNR(5) NOT = ZERO                                     
075400           IF MID-IDRUBNR(2) NUMERIC AND MID-IDRUBNR(2) > ZERO            
075500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(2)               
075600              MOVE JA TO INDATA-FEL                                       
075700           END-IF                                                         
075800           IF MID-IDRUBNR(3) NUMERIC AND MID-IDRUBNR(3) > ZERO            
075900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(3)               
076000              MOVE JA TO INDATA-FEL                                       
076100           END-IF                                                         
076200           IF MID-IDRUBNR(4) NUMERIC AND MID-IDRUBNR(4) > ZERO            
076300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)               
076400              MOVE JA TO INDATA-FEL                                       
076500           END-IF                                                         
076600           IF MID-IDRUBNR(5) NUMERIC AND MID-IDRUBNR(5) > ZERO            
076700              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)               
076800              MOVE JA TO INDATA-FEL                                       
076900           END-IF                                                         
077000        END-IF                                                            
077100     END-IF                                                               
077200                                                                          
077300     IF INDATA-FEL = NEJ                                                  
077400        IF TEST-IDRUBNR(2) NOT = ZERO OR                                  
077500           TEST-IDRUBNR(3) NOT = ZERO                                     
077600           MOVE 'J' TO TEST-FLRUBTYP                                      
077700        ELSE                                                              
077800           IF TEST-IDRUBNR(4) NOT = ZERO OR                               
077900              TEST-IDRUBNR(5) NOT = ZERO                                  
078000              MOVE 'N' TO TEST-FLRUBTYP                                   
078100           ELSE                                                           
078200              IF TEST-IDRUBNR(1) NOT = ZERO                               
078300                 MOVE 'J' TO TEST-FLRUBTYP                                
078400              END-IF                                                      
078500           END-IF                                                         
078600        END-IF                                                            
078700     END-IF                                                               
078800                                                                          
078900*- - - -  - - - - - - - - - - LIKA IDRUBNR                                
079000     IF INDATA-FEL = NEJ                                                  
079100        IF TEST-FLRUBTYP = 'J'                                            
079200           IF TEST-IDRUBNR(1) NOT = ZERO                                  
079300              IF TEST-IDRUBNR(1) = TEST-IDRUBNR(2)                        
079400                 IF MID-IDRUBNR(2) NOT = ALL '+'                          
079500                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(2)         
079600                    MOVE JA TO INDATA-FEL                                 
079700                 ELSE                                                     
079800                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)         
079900                    MOVE JA TO INDATA-FEL                                 
080000                 END-IF                                                   
080100              END-IF                                                      
080200              IF TEST-IDRUBNR(1) = TEST-IDRUBNR(3)                        
080300                 IF MID-IDRUBNR(3) NOT = ALL '+'                          
080400                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(3)         
080500                    MOVE JA TO INDATA-FEL                                 
080600                 ELSE                                                     
080700                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)         
080800                    MOVE JA TO INDATA-FEL                                 
080900                 END-IF                                                   
081000              END-IF                                                      
081100              IF TEST-IDRUBNR(2) NOT = ZERO                               
081200                 IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                     
081300                    IF MID-IDRUBNR(3) NOT = ALL '+'                       
081400                       MOVE MFS-NUM-FAELT-FEL TO                          
081500                            MOD-IDRUBNR-ATTR(3)                           
081600                       MOVE JA TO INDATA-FEL                              
081700                    ELSE                                                  
081800                       MOVE MFS-NUM-FAELT-FEL TO                          
081900                            MOD-IDRUBNR-ATTR(2)                           
082000                       MOVE JA TO INDATA-FEL                              
082100                    END-IF                                                
082200                 END-IF                                                   
082300              END-IF                                                      
082400           ELSE                                                           
082500              IF TEST-IDRUBNR(2) NOT = ZERO                               
082600                 IF TEST-IDRUBNR(2) = TEST-IDRUBNR(3)                     
082700                    IF MID-IDRUBNR(3) NOT = ALL '+'                       
082800                       MOVE MFS-NUM-FAELT-FEL TO                          
082900                            MOD-IDRUBNR-ATTR(3)                           
083000                       MOVE JA TO INDATA-FEL                              
083100                    ELSE                                                  
083200                       MOVE MFS-NUM-FAELT-FEL TO                          
083300                            MOD-IDRUBNR-ATTR(2)                           
083400                       MOVE JA TO INDATA-FEL                              
083500                    END-IF                                                
083600                 END-IF                                                   
083700              END-IF                                                      
083800           END-IF                                                         
083900        ELSE                                                              
084000           IF TEST-IDRUBNR(1) NOT = ZERO                                  
084100              IF TEST-IDRUBNR(1) = TEST-IDRUBNR(4)                        
084200                 IF MID-IDRUBNR(4) NOT = ALL '+'                          
084300                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(4)         
084400                    MOVE JA TO INDATA-FEL                                 
084500                 ELSE                                                     
084600                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)         
084700                    MOVE JA TO INDATA-FEL                                 
084800                 END-IF                                                   
084900              END-IF                                                      
085000              IF TEST-IDRUBNR(1) = TEST-IDRUBNR(5)                        
085100                 IF MID-IDRUBNR(5) NOT = ALL '+'                          
085200                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(5)         
085300                    MOVE JA TO INDATA-FEL                                 
085400                 ELSE                                                     
085500                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRUBNR-ATTR(1)         
085600                    MOVE JA TO INDATA-FEL                                 
085700                 END-IF                                                   
085800              END-IF                                                      
085900              IF TEST-IDRUBNR(4) NOT = ZERO                               
086000                 IF TEST-IDRUBNR(4) = TEST-IDRUBNR(5)                     
086100                    IF MID-IDRUBNR(5) NOT = ALL '+'                       
086200                       MOVE MFS-NUM-FAELT-FEL TO                          
086300                            MOD-IDRUBNR-ATTR(5)                           
086400                       MOVE JA TO INDATA-FEL                              
086500                    ELSE                                                  
086600                       MOVE MFS-NUM-FAELT-FEL TO                          
086700                            MOD-IDRUBNR-ATTR(4)                           
086800                       MOVE JA TO INDATA-FEL                              
086900                    END-IF                                                
087000                 END-IF                                                   
087100              END-IF                                                      
087200           ELSE                                                           
087300              IF TEST-IDRUBNR(4) NOT = ZERO                               
087400                 IF TEST-IDRUBNR(4) = TEST-IDRUBNR(5)                     
087500                    IF MID-IDRUBNR(5) NOT = ALL '+'                       
087600                       MOVE MFS-NUM-FAELT-FEL TO                          
087700                            MOD-IDRUBNR-ATTR(5)                           
087800                       MOVE JA TO INDATA-FEL                              
087900                    ELSE                                                  
088000                       MOVE MFS-NUM-FAELT-FEL TO                          
088100                            MOD-IDRUBNR-ATTR(4)                           
088200                       MOVE JA TO INDATA-FEL                              
088300                    END-IF                                                
088400                 END-IF                                                   
088500              END-IF                                                      
088600           END-IF                                                         
088700        END-IF                                                            
088800     END-IF                                                               
088900     EJECT                                                                
089000     .                                                                    
089100 D-FRAGEBILD    SECTION.                                                  
089200                                                                          
089300     IF IDILLU-WS-N = ZERO                                                
089400       PERFORM DB-HAEMTA-ILLUNR                                           
089500       MOVE IDILLU-WS TO MOD-IDILLU-UT                                    
089600       INSPECT MOD-IDILLU-UT REPLACING LEADING ZERO BY SPACE              
089700     END-IF                                                               
089800                                                                          
089900     IF INDATA-FEL = NEJ                                                  
090000       MOVE IDILLU-WS-N   TO W-IDILLU                                     
090100       IF W-IDILLU = ZERO                                                 
090200          PERFORM I-RENSA-BILD                                            
090300          MOVE JA TO INDATA-FEL                                           
090400       ELSE                                                               
090500          PERFORM IMS-GET-ILLU                                            
090600          IF SEGMENT-SAKNAS                                               
090700             PERFORM I-RENSA-BILD                                         
090800             MOVE JA TO INDATA-FEL                                        
090900          ELSE                                                            
091000             MOVE ILLU-IDCATNR     TO MOD-IDCATNR                         
091100             MOVE ILLU-IDILLU-1    TO MOD-IDILLU-1                        
091200             MOVE ILLU-IDILLU-2    TO MOD-IDILLU-2                        
091300             MOVE ILLU-IDILLU-3    TO MOD-IDILLU-3                        
091400             MOVE ILLU-TIREGDAT    TO MOD-TIREGDAT                        
091500             MOVE ILLU-TIREGDAT-DILLU                                     
091510                                   TO MOD-TIREGDAG-D                      
091600             MOVE ILLU-TENOTE      TO MOD-TENOTE                          
091700             MOVE ILLU-IDMAPP      TO DEL-MAPP                            
091800             MOVE DEL-SUB          TO MOD-IDSUBNR                         
091900             MOVE ILLU-KDFORDON    TO MOD-KDFORDON                        
092000             MOVE ILLU-IDMAPP      TO MOD-IDMAPP                          
092100             MOVE ILLU-IDILLU      TO MOD-IDILLU                          
092200                                                                          
092300             IF ILLU-FLRUBTYP = 'J'                                       
092400                MOVE ILLU-IDRUBNR-1  TO MOD-IDRUBNR (1)                   
092500                MOVE ILLU-IDRUBNR-2  TO MOD-IDRUBNR (2)                   
092600                MOVE ILLU-IDRUBNR-3  TO MOD-IDRUBNR (3)                   
092700                MOVE ZERO            TO MOD-IDRUBNR (4)                   
092800                MOVE ZERO            TO MOD-IDRUBNR (5)                   
092900             ELSE                                                         
093000                MOVE ILLU-IDRUBNR-1  TO MOD-IDRUBNR (1)                   
093100                MOVE ZERO            TO MOD-IDRUBNR (2)                   
093200                MOVE ZERO            TO MOD-IDRUBNR (3)                   
093300                MOVE ILLU-IDRUBNR-2  TO MOD-IDRUBNR (4)                   
093400                MOVE ILLU-IDRUBNR-3  TO MOD-IDRUBNR (5)                   
093500             END-IF                                                       
093600                                                                          
093700             MOVE SPACE TO MOD-BERUBTXT (1)                               
093800             MOVE SPACE TO MOD-BERUBTXT (2)                               
093900             MOVE SPACE TO MOD-BERUBTXT (3)                               
094000                                                                          
094100             IF ILLU-IDRUBNR-1 NOT = ZERO                                 
094200                MOVE ILLU-IDRUBNR-1 TO W-IDRUBNR                          
094300                MOVE +1 TO INDX                                           
094400                PERFORM DA-RUBRIK-TEXT                                    
094500             END-IF                                                       
094600                                                                          
094700             IF ILLU-IDRUBNR-2 NOT = ZERO                                 
094800                MOVE ILLU-IDRUBNR-2 TO W-IDRUBNR                          
094900                MOVE +2 TO INDX                                           
095000                PERFORM DA-RUBRIK-TEXT                                    
095100             END-IF                                                       
095200                                                                          
095300             IF ILLU-IDRUBNR-3 NOT = ZERO                                 
095400                MOVE ILLU-IDRUBNR-3 TO W-IDRUBNR                          
095500                MOVE +3 TO INDX                                           
095600                PERFORM DA-RUBRIK-TEXT                                    
095700             END-IF                                                       
095800                                                                          
095810             MOVE W-IDILLU   TO W-IDILLU-AVSC-MIN                         
095820                                W-IDILLU-AVSC-MAX                         
095850*            --- LÄSER SEKUNDÄR-INDEX WLKATO (WDN5C)                      
095900             PERFORM IMS-GET-NEXT-AVSC-SEQ                                
096100             MOVE +1 TO KOLIND                                            
096110             MOVE +1 TO RADIND                                            
096300             PERFORM UNTIL KOLIND > 4                                     
096310                PERFORM UNTIL RADIND > 8                                  
096400                      IF SEGMENT-FINNS                                    
096500                         MOVE AVSC-IDCATNR   TO RED-CATNR                 
096600                         MOVE AVSC-IDCATGRP  TO RED-CATGRP                
096700                         MOVE AVSC-IDCATAVS  TO RED-CATAVS                
096800                         MOVE AVSC-KDCATPUB (4:3)                         
096900                                            TO  RED-KDCATPUB-R            
097100                         MOVE RED-CATALOG   TO                            
097200                              MOD-KAT-TILL-GRP (KOLIND, RADIND)           
097310                         PERFORM IMS-GET-NEXT-AVSC-SEQ                    
097400                      ELSE                                                
097410                         MOVE SPACE         TO                            
097420                              MOD-KAT-TILL-GRP (KOLIND, RADIND)           
097700                      END-IF                                              
097800                   ADD +1 TO RADIND                                       
097900                END-PERFORM                                               
098000                MOVE +1 TO RADIND                                         
098100                ADD +1 TO KOLIND                                          
098200             END-PERFORM                                                  
098201                                                                          
098210*------VISNING SLUT                                                       
098211                                                                          
098220             IF SEGMENT-FINNS                                             
098222*------UPPDATERA BLÄDDRINGSNYCKLAR                                        
098230                MOVE AVSC-IDCATNR  TO MOD-IDCATNR-NEXT                    
098250                MOVE AVSC-IDCATGRP TO MOD-IDCATGRP-NEXT                   
098270                MOVE AVSC-IDCATAVS TO MOD-IDCATAVS-NEXT                   
098290                MOVE AVSC-KDCATPUB TO MOD-KDCATPUB-NEXT                   
098292             ELSE                                                         
098294                MOVE ZERO    TO MOD-IDCATNR-NEXT                          
098296                                MOD-IDCATGRP-NEXT                         
098298                                MOD-IDCATAVS-NEXT                         
098300                MOVE SPACE   TO MOD-KDCATPUB-NEXT                         
098310             END-IF                                                       
098320          END-IF                                                          
098400       END-IF                                                             
098500     END-IF                                                               
098600     .                                                                    
098700     EJECT                                                                
098800 DA-RUBRIK-TEXT SECTION.                                                  
098900                                                                          
099000     PERFORM IMS-GET-RUB                                                  
099100     IF SEGMENT-FINNS                                                     
099110        IF ENGLISH-TEXT                                                   
099111          MOVE 'GB' TO W-IDSKYLT                                          
099120        ELSE                                                              
099200          MOVE 'S' TO W-IDSKYLT                                           
099210        END-IF                                                            
099300        IF INDX = 1 AND RUB-FLKOMBINERAS = NEJ                            
099400           PERFORM IMS-GET-RUB-TEXT                                       
099500           PERFORM UNTIL SEGMENT-SAKNAS                                   
099600              MOVE TEXT-BERUBTXT TO MOD-BERUBTXT(INDX)                    
099700              ADD +1 TO INDX                                              
099800              PERFORM IMS-GET-RUB-TEXT                                    
099900           END-PERFORM                                                    
100000        ELSE                                                              
100100           PERFORM IMS-GET-RUB-TEXT                                       
100200           IF SEGMENT-FINNS                                               
100300              MOVE TEXT-BERUBTXT TO MOD-BERUBTXT(INDX)                    
100400           END-IF                                                         
100500        END-IF                                                            
100600     ELSE                                                                 
100700        MOVE MTEXT4(SPR-IX) TO MOD-TEMFSINF                               
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 DB-HAEMTA-ILLUNR  SECTION.                                               
101200     SKIP1                                                                
101300     PERFORM IMS-GET-HTR                                                  
101400                                                                          
101500     ADD +1 TO 1210-IDILLU-OVR                                            
101600                                                                          
101700     IF (1210-IDILLU-OVR NOT > 1210-IDILLU-OVR-MAX)                       
101800        CONTINUE                                                          
101900     ELSE                                                                 
102000       MOVE FEL-10 (SPR-IX) TO MOD-TEMFSFEL                               
102100       MOVE JA TO INDATA-FEL                                              
102200     END-IF                                                               
102300                                                                          
102400     .                                                                    
102500     EJECT                                                                
102600 E-BORTTAG    SECTION.                                                    
102700     SKIP1                                                                
102800     PERFORM IMS-GET-NEXT-AVSC-SEQ                                        
102900                                                                          
103000     IF SEGMENT-FINNS                                                     
103100       MOVE JA TO INDATA-FEL                                              
103200     ELSE                                                                 
103300       PERFORM IMS-GET-ILLU                                               
103400       IF SEGMENT-FINNS                                                   
103500         PERFORM IMS-DLET-ILLU                                            
103600         MOVE NEJ TO INDATA-FEL                                           
103700       ELSE                                                               
103800         MOVE JA TO INDATA-FEL                                            
103900       END-IF                                                             
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 F-AENDRING   SECTION.                                                    
104400                                                                          
104500     MOVE IDILLU-WS-N   TO W-IDILLU                                       
104600     PERFORM IMS-GET-ILLU                                                 
104700                                                                          
104800     IF SEGMENT-FINNS                                                     
104900       IF MID-IDCATNR NOT = ALL '+'                                       
105000         MOVE MID-IDCATNR TO ILLU-IDCATNR                                 
105100         MOVE SPAR-FORDON TO ILLU-KDFORDON                                
105200       END-IF                                                             
105300       IF MID-IDILLU-1 NOT = ALL '+'                                      
105400         MOVE MID-IDILLU-1 TO ILLU-IDILLU-1                               
105500       END-IF                                                             
105600       IF MID-IDILLU-2 NOT = ALL '+'                                      
105700         MOVE MID-IDILLU-2 TO ILLU-IDILLU-2                               
105800       END-IF                                                             
105900       IF MID-IDILLU-3 NOT = ALL '+'                                      
106000         MOVE MID-IDILLU-3 TO ILLU-IDILLU-3                               
106100       END-IF                                                             
106200       IF IDRUBNR-FINNS = JA                                              
106300         PERFORM FA-AENDRA-RUBNR                                          
106400       END-IF                                                             
106500       MOVE ILLU-IDMAPP  TO DEL-MAPP                                      
106600       IF MID-IDSUBNR  NOT = '+'                                          
106700         MOVE MID-IDSUBNR  TO DEL-SUB                                     
106800       END-IF                                                             
106900       MOVE ILLU-IDRUBNR-1   TO DEL-RUBNR-N                               
107000       MOVE DEL-RUB12 TO DEL-FORD                                         
107100       MOVE DEL-MAPP     TO ILLU-IDMAPP                                   
107200       IF MID-TENOTE = ALL '+'                                            
107300          CONTINUE                                                        
107400       ELSE                                                               
107500         MOVE MID-TENOTE TO ILLU-TENOTE                                   
107600       END-IF                                                             
107700       PERFORM IMS-REPL-ILLU                                              
107800     ELSE                                                                 
107900       MOVE JA TO INDATA-FEL                                              
108000     END-IF                                                               
108100     .                                                                    
108200     EJECT                                                                
108300 FA-AENDRA-RUBNR  SECTION.                                                
108400                                                                          
108500       MOVE TEST-FLRUBTYP TO ILLU-FLRUBTYP                                
108600         IF TEST-FLRUBTYP = 'J'                                           
108700             MOVE TEST-IDRUBNR (1) TO ILLU-IDRUBNR-1                      
108800             MOVE TEST-IDRUBNR (2) TO ILLU-IDRUBNR-2                      
108900             MOVE TEST-IDRUBNR (3) TO ILLU-IDRUBNR-3                      
109000         ELSE                                                             
109100             MOVE TEST-IDRUBNR (1) TO ILLU-IDRUBNR-1                      
109200             MOVE TEST-IDRUBNR (4) TO ILLU-IDRUBNR-2                      
109300             MOVE TEST-IDRUBNR (5) TO ILLU-IDRUBNR-3                      
109400         END-IF                                                           
109500     .                                                                    
109600     EJECT                                                                
109700 G-NYUPPL  SECTION.                                                       
109800                                                                          
109900       PERFORM GC-NUMMERKOLL                                              
110000                                                                          
110100       IF INDATA-FEL = NEJ AND W-IDILLU > ZERO                            
110200         PERFORM GA-BLANKA-ILLU-AREA                                      
110300         MOVE W-IDILLU      TO ILLU-IDILLU                                
110400         IF MID-IDCATNR NOT = ALL '+'                                     
110500           MOVE MID-IDCATNR TO ILLU-IDCATNR                               
110600         END-IF                                                           
110700         IF MID-IDILLU-1 NOT = ALL '+'                                    
110800           MOVE MID-IDILLU-1 TO ILLU-IDILLU-1                             
110900         END-IF                                                           
111000         IF MID-IDILLU-2 NOT = ALL '+'                                    
111100           MOVE MID-IDILLU-2 TO ILLU-IDILLU-2                             
111200         END-IF                                                           
111300         IF MID-IDILLU-3 NOT = ALL '+'                                    
111400           MOVE MID-IDILLU-3 TO ILLU-IDILLU-3                             
111500         END-IF                                                           
111600         MOVE SPAR-FORDON TO ILLU-KDFORDON                                
111700         IF MID-IDSUBNR NOT = ALL '+'                                     
111800           MOVE MID-IDSUBNR TO DEL-SUB                                    
111900         ELSE                                                             
112000           MOVE ZERO        TO DEL-SUB                                    
112100         END-IF                                                           
112200         IF IDRUBNR-FINNS = JA                                            
112300           PERFORM GB-NYUPPL-RUBNR                                        
112400         END-IF                                                           
112500                                                                          
112600         MOVE DAGENS-DATUM TO ILLU-TIREGDAT                               
112700                                                                          
112800         IF MID-TENOTE NOT = ALL '+'                                      
112900           MOVE MID-TENOTE TO ILLU-TENOTE                                 
113000         END-IF                                                           
113100         PERFORM IMS-ISRT-ILLU                                            
113200         MOVE IDILLU-WS TO MOD-IDILLU-UT                                  
113300         INSPECT MOD-IDILLU-UT REPLACING LEADING ZERO BY SPACE            
113400       END-IF                                                             
113500     .                                                                    
113600     EJECT                                                                
113700 GA-BLANKA-ILLU-AREA  SECTION.                                            
113800                                                                          
113900     MOVE  ZERO   TO   ILLU-IDILLU                                        
114000                       ILLU-IDCATNR                                       
114100                       ILLU-IDILLU-1                                      
114200                       ILLU-IDILLU-2                                      
114300                       ILLU-IDILLU-3                                      
114400                       ILLU-IDMAPP                                        
114500                       ILLU-IDRUBNR-1                                     
114600                       ILLU-IDRUBNR-2                                     
114700                       ILLU-IDRUBNR-3                                     
114800                       ILLU-TIREGDAT-DILLU                                
114900     MOVE SPACE   TO   ILLU-KDFORDON                                      
115000                       ILLU-FLRUBTYP                                      
115100                       ILLU-TENOTE                                        
115200     .                                                                    
115300     EJECT                                                                
115400 GB-NYUPPL-RUBNR  SECTION.                                                
115500                                                                          
115600       IF TEST-IDRUBNR (1) NOT = ZERO                                     
115700         MOVE TEST-IDRUBNR (1) TO DEL-RUBNR                               
115800         MOVE DEL-RUB12 TO DEL-FORD                                       
115900       ELSE                                                               
116000         MOVE ZERO TO DEL-FORD                                            
116100       END-IF                                                             
116200       MOVE DEL-MAPP TO ILLU-IDMAPP                                       
116300       MOVE TEST-FLRUBTYP TO ILLU-FLRUBTYP                                
116400       IF ILLU-FLRUBTYP = 'J'                                             
116500           MOVE TEST-IDRUBNR (1) TO ILLU-IDRUBNR-1                        
116600           MOVE TEST-IDRUBNR (2) TO ILLU-IDRUBNR-2                        
116700           MOVE TEST-IDRUBNR (3) TO ILLU-IDRUBNR-3                        
116800       ELSE                                                               
116900           MOVE TEST-IDRUBNR (1) TO ILLU-IDRUBNR-1                        
117000           MOVE TEST-IDRUBNR (4) TO ILLU-IDRUBNR-2                        
117100           MOVE TEST-IDRUBNR (5) TO ILLU-IDRUBNR-3                        
117200       END-IF                                                             
117300     .                                                                    
117400     EJECT                                                                
117500 GC-NUMMERKOLL  SECTION.                                                  
117600                                                                          
117700     PERFORM IMS-GET-HTR                                                  
117800     MOVE IDILLU-WS-N TO W-IDILLU                                         
117900                                                                          
118000     IF SPAR-FORDON = 'PV'  OR 'RE'                                       
118100       ADD +1 TO 1210-IDILLU-OVR                                          
118200       MOVE 1210-IDILLU-OVR TO W-IDILLU                                   
118300       MOVE W-IDILLU TO IDILLU-WS-N                                       
118400       IF (W-IDILLU NOT > 1210-IDILLU-OVR-MAX)                            
118500             PERFORM IMS-REPL-HTR                                         
118600       ELSE                                                               
118700           MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                             
118800           MOVE JA TO INDATA-FEL                                          
118900       END-IF                                                             
119000     ELSE                                                                 
119100       IF SPAR-FORDON = 'BR'                                              
119200          IF (W-IDILLU NOT > 1210-IDILLU-BR-MAX)                          
119300             CONTINUE                                                     
119310              MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                          
119320              MOVE JA TO INDATA-FEL                                       
119400          ELSE                                                            
119500              MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                          
119600              MOVE JA TO INDATA-FEL                                       
119700          END-IF                                                          
119800       ELSE                                                               
119900          IF SPAR-FORDON = 'NL'                                           
120000             IF (W-IDILLU NOT > 1210-IDILLU-W-MAX)                        
120100                CONTINUE                                                  
120200             ELSE                                                         
120300                 MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                       
120400                 MOVE JA TO INDATA-FEL                                    
120500             END-IF                                                       
120600          ELSE                                                            
120700             MOVE FEL-8(SPR-IX) TO MOD-TEMFSFEL                           
120800             MOVE JA TO INDATA-FEL                                        
120900          END-IF                                                          
121000       END-IF                                                             
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400 H-VISA-BILD-IGEN  SECTION.                                               
121500                                                                          
121600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATNR                                
121700                               MOD-IDILLU-1                               
121800                               MOD-IDILLU-2                               
121900                               MOD-IDILLU-3                               
122000                               MOD-IDSUBNR                                
122100                               MOD-KDFORDON                               
122200                               MOD-IDMAPP                                 
122300                               MOD-IDILLU                                 
122400                               MOD-IDRUBNR (1)                            
122500                               MOD-IDRUBNR (2)                            
122600                               MOD-IDRUBNR (3)                            
122700                               MOD-IDRUBNR (4)                            
122800                               MOD-IDRUBNR (5)                            
122900                               MOD-BERUBTXT  (1)                          
123000                               MOD-BERUBTXT  (2)                          
123100                               MOD-BERUBTXT  (3)                          
123200                               MOD-TIREGDAT                               
123300                               MOD-TIREGDAG-D                             
123400                               MOD-TENOTE                                 
123500                                                                          
123700     MOVE +1 TO KOLIND                                                    
123710     MOVE +1 TO RADIND                                                    
123900     PERFORM UNTIL KOLIND > 4                                             
123910       PERFORM UNTIL RADIND > 8                                           
124000         MOVE MFS-ROER-EJ-FAELT                                           
124100          TO MOD-KAT-TILL-GRP (KOLIND, RADIND)                            
124200         ADD +1 TO RADIND                                                 
124300       END-PERFORM                                                        
124400       MOVE +1 TO RADIND                                                  
124500       ADD +1 TO KOLIND                                                   
124600     END-PERFORM                                                          
124700     .                                                                    
124800     EJECT                                                                
124900 I-RENSA-BILD      SECTION.                                               
125000                                                                          
125100     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR                                  
125200                             MOD-IDILLU-1                                 
125300                             MOD-IDILLU-2                                 
125400                             MOD-IDILLU-3                                 
125500                             MOD-IDSUBNR                                  
125600                             MOD-KDFORDON                                 
125700                             MOD-IDMAPP                                   
125800                             MOD-IDILLU                                   
125900                             MOD-IDRUBNR (1)                              
126000                             MOD-IDRUBNR (2)                              
126100                             MOD-IDRUBNR (3)                              
126200                             MOD-IDRUBNR (4)                              
126300                             MOD-IDRUBNR (5)                              
126400                             MOD-BERUBTXT  (1)                            
126500                             MOD-BERUBTXT  (2)                            
126600                             MOD-BERUBTXT  (3)                            
126700                             MOD-TIREGDAT                                 
126800                             MOD-TIREGDAG-D                               
126900                             MOD-TENOTE                                   
127000                                                                          
127200     MOVE +1 TO KOLIND                                                    
127210     MOVE +1 TO RADIND                                                    
127400     PERFORM UNTIL KOLIND > 4                                             
127410       PERFORM UNTIL RADIND > 8                                           
127500         MOVE MFS-RENSA-FAELT                                             
127600          TO MOD-KAT-TILL-GRP (KOLIND, RADIND)                            
127700         ADD +1 TO RADIND                                                 
127800       END-PERFORM                                                        
127900       MOVE +1 TO RADIND                                                  
128000       ADD +1 TO KOLIND                                                   
128100     END-PERFORM                                                          
128200     .                                                                    
128300     EJECT                                                                
128400 J-GRUND-FORMAT    SECTION.                                               
128500     SKIP1                                                                
128600     MOVE MFS-FORMATETS-ATTR TO MOD-IDCATNR-ATTR                          
128700                                MOD-IDILLU-1-ATTR                         
128800                                MOD-IDILLU-2-ATTR                         
128900                                MOD-IDILLU-3-ATTR                         
129000                                MOD-IDSUBNR-ATTR                          
129100                                MOD-IDRUBNR-ATTR (1)                      
129200                                MOD-IDRUBNR-ATTR (2)                      
129300                                MOD-IDRUBNR-ATTR (3)                      
129400                                MOD-IDRUBNR-ATTR (4)                      
129500                                MOD-IDRUBNR-ATTR (5)                      
129600                                MOD-TENOTE-ATTR                           
129700                                                                          
129800     .                                                                    
129900     EJECT                                                                
129910 K-NAESTA-SIDA    SECTION.                                                
129920     SKIP1                                                                
129922     MOVE IDILLU-WS-N TO  W-IDILLU  W-IDILLU-AVSC                         
129923                                    W-IDILLU-AVSC-MIN                     
129924                                    W-IDILLU-AVSC-MAX                     
129925     IF MID-IDCATNR-NEXT > ZERO                                           
129931        MOVE MID-KDCATPUB-NEXT TO W-KDCATPUB-AVSC                         
129932        MOVE MID-IDCATNR-NEXT  TO W-IDCATNR-AVSC                          
129933        MOVE MID-IDCATGRP-NEXT TO W-IDCATGRP-AVSC                         
129934        MOVE MID-IDCATAVS-NEXT TO W-IDCATAVS-AVSC                         
129935        PERFORM IMS-GET-AVSC-SEQ                                          
129936     ELSE                                                                 
129939        PERFORM IMS-GET-NEXT-AVSC-SEQ                                     
129940     END-IF                                                               
129941                                                                          
129942     MOVE +1 TO KOLIND                                                    
129943     MOVE +1 TO RADIND                                                    
129944     PERFORM UNTIL KOLIND > 4                                             
129945        PERFORM UNTIL RADIND > 8                                          
129946           IF SEGMENT-FINNS                                               
129947              MOVE AVSC-IDCATNR   TO   RED-CATNR                          
129948              MOVE AVSC-IDCATGRP  TO   RED-CATGRP                         
129949              MOVE AVSC-IDCATAVS  TO   RED-CATAVS                         
129950              MOVE AVSC-KDCATPUB (4:3)                                    
129951                                  TO   RED-KDCATPUB-R                     
129952              MOVE RED-CATALOG    TO                                      
129953                   MOD-KAT-TILL-GRP (KOLIND, RADIND)                      
129954              PERFORM IMS-GET-NEXT-AVSC-SEQ                               
129955           ELSE                                                           
129956              MOVE SPACE          TO                                      
129957                   MOD-KAT-TILL-GRP (KOLIND, RADIND)                      
129958           END-IF                                                         
129959           ADD +1 TO RADIND                                               
129960        END-PERFORM                                                       
129961        MOVE +1 TO RADIND                                                 
129962        ADD +1 TO KOLIND                                                  
129963     END-PERFORM                                                          
129964                                                                          
129965*---------VISNING SLUT                                                    
129966                                                                          
129967     IF SEGMENT-FINNS                                                     
129969*---------UPPDATERA BLÄDDRINGSNYCKLAR                                     
129971        MOVE AVSC-IDCATNR  TO MOD-IDCATNR-NEXT                            
129973        MOVE AVSC-IDCATGRP TO MOD-IDCATGRP-NEXT                           
129975        MOVE AVSC-IDCATAVS TO MOD-IDCATAVS-NEXT                           
129977        MOVE AVSC-KDCATPUB TO MOD-KDCATPUB-NEXT                           
129979     ELSE                                                                 
129980        MOVE ZERO    TO MOD-IDCATNR-NEXT                                  
129981                        MOD-IDCATGRP-NEXT                                 
129982                        MOD-IDCATAVS-NEXT                                 
129983        MOVE SPACE   TO MOD-KDCATPUB-NEXT                                 
129984     END-IF                                                               
129985                                                                          
129986     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATNR                                
129987                               MOD-IDILLU-1                               
129988                               MOD-IDILLU-2                               
129989                               MOD-IDILLU-3                               
129990                               MOD-IDSUBNR                                
129991                               MOD-KDFORDON                               
129992                               MOD-IDMAPP                                 
129993                               MOD-IDILLU                                 
129994                               MOD-IDRUBNR (1)                            
129995                               MOD-IDRUBNR (2)                            
129996                               MOD-IDRUBNR (3)                            
129997                               MOD-IDRUBNR (4)                            
129998                               MOD-IDRUBNR (5)                            
129999                               MOD-BERUBTXT  (1)                          
130000                               MOD-BERUBTXT  (2)                          
130001                               MOD-BERUBTXT  (3)                          
130002                               MOD-TIREGDAT                               
130003                               MOD-TIREGDAG-D                             
130004                               MOD-TENOTE                                 
130005                                                                          
130019     .                                                                    
130020     EJECT                                                                
130030 S01-KOLLA-RUB-MED-BAS SECTION.                                           
130100     SKIP2                                                                
130200     MOVE +1 TO INDX                                                      
130300     PERFORM UNTIL INDX NOT < 6                                           
130400        IF IN-IDRUBNR(INDX) NOT = ZERO                                    
130500           MOVE MID-IDRUBNR(INDX) TO W-IDRUBNR                            
130600           PERFORM IMS-GET-RUB                                            
130700           IF SEGMENT-FINNS                                               
130800              IF (TEST-IDRUBNR(2) NOT = ZERO) OR                          
130900                 (TEST-IDRUBNR(3) NOT = ZERO) OR                          
131000                 (TEST-IDRUBNR(4) NOT = ZERO) OR                          
131100                 (TEST-IDRUBNR(5) NOT = ZERO)                             
131200                 IF RUB-FLKOMBINERAS = ' ' OR 'N'                         
131300                    MOVE MFS-NUM-FAELT-FEL TO                             
131400                         MOD-IDRUBNR-ATTR(INDX)                           
131500                    MOVE JA TO INDATA-FEL                                 
131600                 END-IF                                                   
131700              END-IF                                                      
131800           ELSE                                                           
131900              MOVE MFS-NUM-FAELT-FEL TO                                   
132000                   MOD-IDRUBNR-ATTR(INDX)                                 
132100              MOVE JA TO INDATA-FEL                                       
132200           END-IF                                                         
132300        END-IF                                                            
132400        ADD +1 TO INDX                                                    
132500     END-PERFORM                                                          
132600     .                                                                    
132700     EJECT                                                                
132800* IMS SEKTIONER                                                           
132900     SKIP3                                                                
133000 IMS-GET-MSG SECTION.                                                     
133100     MOVE '  QC' TO GODK-STATUSKODER                                      
133200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
133300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     SKIP3                                                                
133700 IMS-INSERT-MSG SECTION.                                                  
133800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
133900     MOVE SPACE TO GODK-STATUSKODER                                       
134000     IF ENGLISH-TEXT                                                      
134100        MOVE 'N' TO MFS-KDHUVOMR                                          
134200     END-IF                                                               
134300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
134400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134500     PERFORM IMS-STATUSKONTROLL                                           
134600     .                                                                    
134700     EJECT                                                                
134800 IMS-GET-ILLU    SECTION.                                                 
134900     STRING 'WLKATK01(IDILLU   =' W-IDILLU-X ')'                          
135000            DELIMITED BY SIZE INTO SSA1                                   
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GHU ILLU-PCB IO-AREA SSA1                         
135300     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     SKIP3                                                                
135700 IMS-GET-AVSC-SEQ SECTION.                                                
135800     STRING 'WLKATO01(WDN5C1KY =' W-WDN5C1KY-X   ')'                      
135900            DELIMITED BY SIZE INTO SSA1                                   
136100     MOVE '  GE' TO GODK-STATUSKODER                                      
136200     CALL CBLTDLI USING GU KATO-PCB IO-AREA3 SSA1                         
136300     MOVE KATO-STATUS-CODE TO STATUS-WS                                   
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136691     SKIP3                                                                
136692 IMS-GET-NEXT-AVSC-SEQ SECTION.                                           
136693     STRING 'WLKATO01(WDN5C1KY>=' W-WDN5C1KY-MIN-X                        
136695                 OCH 'WDN5C1KY<=' W-WDN5C1KY-MAX-X ')'                    
136700            DELIMITED BY SIZE INTO SSA1                                   
136701     MOVE '  GE' TO GODK-STATUSKODER                                      
136702     CALL CBLTDLI USING GN KATO-PCB IO-AREA3 SSA1                         
136703     MOVE KATO-STATUS-CODE TO STATUS-WS                                   
136704     PERFORM IMS-STATUSKONTROLL                                           
136705     .                                                                    
136706     SKIP3                                                                
136710 IMS-REPL-ILLU SECTION.                                                   
136800     MOVE SPACE  TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING REPL ILLU-PCB IO-AREA                             
137000     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300     SKIP3                                                                
137400 IMS-DLET-ILLU SECTION.                                                   
137500     MOVE SPACE  TO GODK-STATUSKODER                                      
137600     CALL CBLTDLI USING DLET ILLU-PCB IO-AREA                             
137700     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
137800     PERFORM IMS-STATUSKONTROLL                                           
137900     .                                                                    
138000     SKIP3                                                                
138100 IMS-ISRT-ILLU SECTION.                                                   
138200     MOVE 'WLKATK01 ' TO SSA1                                             
138300     MOVE SPACE  TO GODK-STATUSKODER                                      
138400     CALL CBLTDLI USING ISRT ILLU-PCB IO-AREA SSA1                        
138500     MOVE ILLU-STATUS-CODE TO STATUS-WS                                   
138600     PERFORM IMS-STATUSKONTROLL                                           
138700     .                                                                    
138800     EJECT                                                                
138900 IMS-GET-KAT     SECTION.                                                 
139000     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
139100            DELIMITED BY SIZE INTO SSA1                                   
139200     MOVE '  GE' TO GODK-STATUSKODER                                      
139300     CALL CBLTDLI USING GU KAT-PCB IO-AREA2 SSA1                          
139400     MOVE KAT-STATUS-CODE TO STATUS-WS                                    
139500     PERFORM IMS-STATUSKONTROLL                                           
139600     .                                                                    
139700     EJECT                                                                
139800 IMS-GET-RUB     SECTION.                                                 
139900     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
140000            DELIMITED BY SIZE INTO SSA1                                   
140100     MOVE '  GE' TO GODK-STATUSKODER                                      
140200     CALL CBLTDLI USING GU RUB-PCB IO-AREA2 SSA1                          
140300     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     SKIP3                                                                
140700 IMS-GET-RUB-TEXT    SECTION.                                             
140800     STRING 'WLKATB11(IDSKYLT  =' W-IDSKYLT ')'                           
140900            DELIMITED BY SIZE INTO SSA1                                   
141000     MOVE '  GE' TO GODK-STATUSKODER                                      
141100     CALL CBLTDLI USING GNP RUB-PCB IO-AREA2 SSA1                         
141200     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
141300     PERFORM IMS-STATUSKONTROLL                                           
141400     .                                                                    
141500     EJECT                                                                
141600 IMS-GET-HTR     SECTION.                                                 
141700     STRING 'WLXXAJ01(WDGXKEY  =' W-1209-KEY-X ')'                        
141800            DELIMITED BY SIZE INTO SSA1                                   
141900     MOVE 'WLXXAJ11 ' TO SSA2                                             
142000     MOVE '    ' TO GODK-STATUSKODER                                      
142100     CALL CBLTDLI USING GHU HTR-PCB IO-AREA2 SSA1 SSA2                    
142200     MOVE HTR-STATUS-CODE TO STATUS-WS                                    
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP2                                                                
142600 IMS-REPL-HTR  SECTION.                                                   
142700     MOVE SPACE  TO GODK-STATUSKODER                                      
142800     CALL CBLTDLI USING REPL    HTR-PCB IO-AREA2                          
142900     MOVE  HTR-STATUS-CODE TO STATUS-WS                                   
143000     PERFORM IMS-STATUSKONTROLL                                           
143100     .                                                                    
143200     EJECT                                                                
143300 IMS-STATUSKONTROLL SECTION.                                              
143400     SET STATUS-IX TO 1                                                   
143500     SEARCH GODK-STATUS AT END CALL FELLOG                                
143600         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
143700             CONTINUE                                                     
143800     END-SEARCH                                                           
143900     .                                                                    
