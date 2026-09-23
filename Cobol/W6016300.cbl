000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W6016300.                                                 
000300 AUTHOR.        INGRID DANIELSSON/E RINGQVIST/TOMMIE JIVARP               
000400 DATE-WRITTEN.  FEB. 1980.       /MAJ 1984.  /MARS 1998.                  
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        TP-PROGRAM FÖR ÄNDRING AV ARTIKELREGISTRETS (WDD1)               
000800*        FÖRRÅDS-SEGMENT.                                                 
000900*                                                                         
001000*        SAMT UPPDATERING AV SALDOBASEN (WDD8) MED BORTTAG ELLER          
001100*        NYUPPLÄGG ELLER ENDAST ÄNDRING AV BUFFERTADRESS,                 
001200*                                                                         
001300*        OCH, UPPDATERING AV PLATSREGISTER-BASEN (WDJ9)                   
001400*                                                                         
001500*        STARTAR EV DISPATCH FÖR UPPDATERING AV ART INFO PÅ               
001600*        INLEVERANSREGISTRET.                                             
001700*                                                                         
001800*        PF4 FÖR UTSKRIFT AV PLATSSÄTTARLISTA                             
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W6T163                                              
002300*        MID:         W6I16301                                            
002400*    UTDATA.                                                              
002500*        MOD:         W6O16301                                            
002600*    SUBPROGRAM.                                                          
002700*        FELLOG                                                           
002800*        W006KOM  (DISPATCH)                                              
002900*                                                                         
003000*    E-TRACKER: 081201 7450328/7450319  VOHF                              
003100*    E-TRACKER: 2015   10254592         DECOMISSION VOHF                  
003200*    JIRA: 180321  1967 - JIRA CASE     INCLDUE AREA 11                   
003300*    JIRA: 180611  2564 - JIRA CASE     ADD MAX STOCK TO AREA             
003400*                                       20, 21 AND 22                     
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800*                                                                         
003900 DATA DIVISION.                                                           
004000                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77      IDARTNR-WS         PIC X(9).                                     
004500 77      JA                 PIC X                 VALUE 'J'.              
004600 77      YES                PIC X                 VALUE 'Y'.              
004700 77      NEJ                PIC X                 VALUE 'N'.              
004800 77      FEL                PIC X                 VALUE 'F'.              
004900 77      BUFFOMR-OK-1       PIC X                 VALUE 'J'.              
005000 77      BUFFOMR-OK-2       PIC X                 VALUE 'J'.              
005100 77      BUFFOMR-OK-3       PIC X                 VALUE 'J'.              
005200 77      INGET-IFYLLT       PIC X                 VALUE 'I'.              
005300 77      RETT               PIC X                 VALUE 'R'.              
005400 77      MAX-MODLAENGD      PIC S9(4) COMP SYNC   VALUE +412.             
005500 77      RAD-MAX            PIC S9(3) COMP-3      VALUE +26.              
005600 77      IX-BUFF            PIC S9(1) COMP-3      VALUE +0.               
005700 77      IX-FEL             PIC S9(1) COMP-3      VALUE +0.               
005800 77      COUNTR             PIC S9(3)             VALUE +0.               
005900 77      DAGENS-DATUM       PIC 9(6)              VALUE ZERO.             
006000 77      DAGENS-TID         PIC 9(8)              VALUE ZERO.             
006100 77      LNG-P-TO-P-PREFIX  PIC S9(4) COMP SYNC   VALUE +17.              
006200 77      LOGG-DATUM         PIC S9(8)             VALUE ZERO.             
006300 77      LOGG-TID           PIC S9(7)             VALUE ZERO.             
006400 77      PRIME-LOCATION-CDC PIC X                 VALUE 'C'.              
006500 77      PRIME-LOCATION-SVS PIC X                 VALUE 'S'.              
006600 77      W-ADLAGOMR-WDJ9-CDC                                              
006700                            PIC 9(2)              VALUE ZERO.             
006800 77      W-ADGANG-WDJ9-CDC  PIC 9(2)              VALUE ZERO.             
006900 77      W-ADPLATS-WDJ9-CDC PIC 9(5)              VALUE ZERO.             
007000 77      W-ADLAGOMR-WDJ9-SVS                                              
007100                            PIC 9(2)              VALUE ZERO.             
007200 77      W-ADGANG-WDJ9-SVS  PIC 9(2)              VALUE ZERO.             
007300 77      W-ADPLATS-WDJ9-SVS PIC 9(5)              VALUE ZERO.             
007400 77      WS-ADLAGOMR        PIC 9(3)              VALUE ZERO.             
007500 77      WS-ADGANG          PIC 9(3)              VALUE ZERO.             
007600 77      WS-ADPLATS         PIC 9(5)              VALUE ZERO.             
007700 77  WC-BEFORE                    PIC X       VALUE 'N'.                  
007800 77  WC-AFTER                     PIC X       VALUE 'N'.                  
007900 77  WC-NOLL                      PIC X       VALUE 'N'.                  
008000 77  WC-NOLL-WDK6                 PIC X       VALUE 'N'.                  
009000                                                                          
009100 77  ADLAGOMR-SW                  PIC X       VALUE 'J'.                  
009200     88  ADLAGOMR-OK                          VALUE 'J'.                  
009300     88  ADLAGOMR-FEL                         VALUE 'N'.                  
009400 77  WX-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                 
009500 77  WY-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                 
009600                                                                          
009700                                                                          
009800*01  SPARA-VLARTNTO             PIC S9(8)V9(1)   COMP-3.                  
009900*01  SPARA-VKART                PIC S9(7)        COMP-3.                  
010000 01  WS-VLARTNTO             PIC 9(8)V9(1).                               
010100 01  WS-VLARTNTO-XX  REDEFINES WS-VLARTNTO.                               
010200     05 WS-VLARTNTO-ALFA     PIC X(9).                                    
010300                                                                          
010400*      --- VALID IDDC CODES                                               
010500*                                                                         
010600*01    -COPY WWDCKONS                                                     
010700*                                                                         
010800*01    -COPY WWLNDKON                                                     
010900       EJECT                                                              
011000                                                                          
011100 01  TEST-KDVSOP.                                                         
011200     03  KDVSOP-1        PIC 9.                                           
011300     03  KDVSOP-2        PIC 9.                                           
011400     03  KDVSOP-3        PIC 9.                                           
011500                                                                          
011600                                                                          
011700 01  FILLER                  PIC X(16)  VALUE 'NYCKLAR-TILL-DLI'.         
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-IDARTNR-X.                                                     
012000         05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.              
012100     03  W-IDARTNR2-X.                                                    
012200         05  W-IDARTNR2      PIC S9(9)   VALUE ZERO  COMP-3.              
012300     03  W-IDDC-X.                                                        
012400         05  W-IDDC          PIC  X(2)   VALUE '11'.                      
012500     03  W-IDLEVNR-X.                                                     
012600         05  W-IDLEVNR       PIC  X(5)   VALUE SPACE.                     
012700     03  W-KDSEGKEY-X.                                                    
012800         05  W-KDSEGKEY      PIC  X(1)   VALUE '1'.                       
012900     03  W-WDD811KY-X.                                                    
013000         05  W-IDDC-WDD8     PIC  X(2)   VALUE '11'.                      
013100         05  W-ADBUFFOMR     PIC S9(3)   VALUE ZERO  COMP-3.              
013200         05  W-DABUFPAF      PIC  9(8)   VALUE ZERO.                      
013300         05  W-ADBUFFGANG    PIC S9(3)   VALUE ZERO  COMP-3.              
013400         05  W-ADBUFFPL      PIC S9(5)   VALUE ZERO  COMP-3.              
013500     03  W-W6GXKEY-X.                                                     
013600         05  FILLER          PIC X(4)    VALUE '6005'.                    
013700         05  FILLER          PIC X(2)    VALUE '11'.                      
013800         05  FILLER          PIC X(24)   VALUE LOW-VALUE.                 
013900     03  W-ADINLOMR-X.                                                    
014000         05  W-ADINLOMR      PIC X(4)    VALUE SPACE.                     
014100     03  W-WDJ911KY-X.                                                    
014200         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
014300         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
014400         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
014500         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
014600         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
014700         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
014800     03  W-IDLAND-X.                                                      
014900         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
015000     EJECT                                                                
015100 01      MEDDELANDE.                                                      
015200                                                                          
015300     03  RETT1.                                                           
015400         05  FILLER  PIC X(40)                                            
015500             VALUE 'UPPLYSTA FÄLT REGISTRERADE             '.             
015600         05  FILLER  PIC X(40)                                            
015700             VALUE 'LIGHTENED FIELDS REGISTRATED           '.             
015800     03  FILLER REDEFINES RETT1.                                          
015900         05  RETT-1  PIC X(40)  OCCURS 2.                                 
016000                                                                          
016100     03  RETT2.                                                           
016200         05  FILLER  PIC X(40)                                            
016300             VALUE 'TRYCK PF11 FÖR UPPDATERING             '.             
016400         05  FILLER  PIC X(40)                                            
016500             VALUE 'PRESS PF11 WHEN UPDATE                 '.             
016600     03  FILLER REDEFINES RETT2.                                          
016700         05  RETT-2  PIC X(40)  OCCURS 2.                                 
016800                                                                          
016900     03  RETT3.                                                           
017000         05  FILLER  PIC X(40)                                            
017100             VALUE 'PF11 OCH TOM INDATARAD                 '.             
017200         05  FILLER  PIC X(40)                                            
017300             VALUE 'PF11 AND NO INPUT                      '.             
017400     03  FILLER REDEFINES RETT3.                                          
017500         05  RETT-3  PIC X(40)  OCCURS 2.                                 
017600                                                                          
017700     03  RETT4.                                                           
017800         05  FILLER  PIC X(40)                                            
017900             VALUE 'PRINTNING BEGÄRD                       '.             
018000         05  FILLER  PIC X(40)                                            
018100             VALUE 'PRINTNING REQUESTED                    '.             
018200     03  FILLER REDEFINES RETT4.                                          
018300         05  RETT-4  PIC X(40)  OCCURS 2.                                 
018400                                                                          
018500     03  FEL1.                                                            
018600         05  FILLER PIC X(40)                                             
018700             VALUE 'ARTIKELNUMRET EJ NUMERISKT            '.              
018800         05  FILLER PIC X(40)                                             
018900             VALUE 'PART NO NOT NUMERIC                    '.             
019000     03  FILLER REDEFINES FEL1.                                           
019100         05  FEL-1  PIC X(40)  OCCURS 2.                                  
019200                                                                          
019300     03  FEL2.                                                            
019400         05  FILLER PIC X(40)                                             
019500             VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET   '.             
019600         05  FILLER PIC X(40)                                             
019700             VALUE 'PART NO NOT IN THE DATABASE            '.             
019800     03  FILLER REDEFINES FEL2.                                           
019900         05  FEL-2  PIC X(40)  OCCURS 2.                                  
020000                                                                          
020100     03  FEL3.                                                            
020200         05  FILLER PIC X(40)                                             
020300             VALUE 'ARTIKELN ÄR UTGÅNGEN                   '.             
020400         05  FILLER PIC X(40)                                             
020500             VALUE 'PART NO HAS BEEN DELETED               '.             
020600     03  FILLER REDEFINES FEL3.                                           
020700         05  FEL-3  PIC X(40)  OCCURS 2.                                  
020800     EJECT                                                                
020900     03  FEL4.                                                            
021000         05  FILLER PIC X(40)                                             
021100             VALUE 'FÖRRÅDS INFORMATION SAKNAS             '.             
021200         05  FILLER PIC X(40)                                             
021300             VALUE 'STOCK INFO MISSING                     '.             
021400     03  FILLER REDEFINES FEL4.                                           
021500         05  FEL-4  PIC X(40)  OCCURS 2.                                  
021600                                                                          
021700     03  FEL5.                                                            
021800         05  FILLER PIC X(40)                                             
021900             VALUE 'UPPLYSTA FÄLT FEL                      '.             
022000         05  FILLER PIC X(40)                                             
022100             VALUE 'LIGHTENED FIELDS WRONG                 '.             
022200     03  FILLER REDEFINES FEL5.                                           
022300         05  FEL-5  PIC X(40)  OCCURS 2.                                  
022400                                                                          
022500     03  FEL6.                                                            
022600         05  FILLER PIC X(40)                                             
022700             VALUE 'BUFFERTKVANT EJ LIKA MED 0 OCH OMR ÄR 0'.             
022800         05  FILLER PIC X(40)                                             
022900             VALUE 'BUFFERQTY NOT EQUAL 0 AND AREA EQUAL 0 '.             
023000     03  FILLER REDEFINES FEL6.                                           
023100         05  FEL-6  PIC X(40)  OCCURS 2.                                  
023200                                                                          
023300     03  FEL7.                                                            
023400         05  FILLER PIC X(40)                                             
023500             VALUE 'OMRÅDE ÄR 1, GÅNG OCH PLATS ÄR EJ 0    '.             
023600         05  FILLER PIC X(40)                                             
023700             VALUE 'AREA EQUAL 1, PATH AND LOCATION NOT 0  '.             
023800     03  FILLER REDEFINES FEL7.                                           
023900         05  FEL-7  PIC X(40)  OCCURS 2.                                  
024000                                                                          
024100     03  FEL8.                                                            
024200         05  FILLER PIC X(40)                                             
024300             VALUE 'KVANTITET ÄR EJ NOLL                   '.             
024400         05  FILLER PIC X(40)                                             
024500             VALUE 'QUANTITY NOT EQUAL ZERO                '.             
024600     03  FILLER REDEFINES FEL8.                                           
024700         05  FEL-8  PIC X(40)  OCCURS 2.                                  
024800     EJECT                                                                
024900     03  FEL9.                                                            
025000         05  FILLER PIC X(40)                                             
025100             VALUE 'PRINTER SAKNAS                         '.             
025200         05  FILLER PIC X(40)                                             
025300             VALUE 'PRINTER MISSING                        '.             
025400     03  FILLER REDEFINES FEL9.                                           
025500         05  FEL-9  PIC X(40)  OCCURS 2.                                  
025600     EJECT                                                                
025700     03  FEL10.                                                           
025800         05  FILLER PIC X(40)                                             
025900             VALUE 'LO SAKNAS, ANVÄND BILD 6106.           '.             
026000         05  FILLER PIC X(40)                                             
026100             VALUE 'LO MISSING, USE SCREEN 6106.           '.             
026200     03  FILLER REDEFINES FEL10.                                          
026300         05  FEL-10 PIC X(40)  OCCURS 2.                                  
026400     EJECT                                                                
026500     03  FEL11.                                                           
026600         05  FILLER PIC X(40)                                             
026700             VALUE 'ENDAST LO 42 ELLER 43 TILLÅTEN         '.             
026800         05  FILLER PIC X(40)                                             
026900             VALUE 'ONLY LO 42 OR 43 ALLOWED               '.             
027000     03  FILLER REDEFINES FEL11.                                          
027100         05  FEL-11 PIC X(40)  OCCURS 2.                                  
027200     EJECT                                                                
027300     03  FEL12.                                                           
027400         05  FILLER PIC X(40)                                             
027500             VALUE 'HF PÅFYLLNING FINNS                    '.             
027600         05  FILLER PIC X(40)                                             
027700             VALUE 'HF REFILL PROPOSAL EXISTS              '.             
027800     03  FILLER REDEFINES FEL12.                                          
027900         05  FEL-12 PIC X(40)  OCCURS 2.                                  
028000     EJECT                                                                
028100     03  FEL13.                                                           
028200         05  FILLER PIC X(40)                                             
028300             VALUE 'MISSING IN REGISTER                    '.             
028400         05  FILLER PIC X(40)                                             
028500             VALUE 'MISSING IN REGISTER                    '.             
028600     03  FILLER REDEFINES FEL13.                                          
028700         05  FEL-13 PIC X(40)  OCCURS 2.                                  
028800     EJECT                                                                
028900 01  FILLER                  PIC X(16)  VALUE 'SWITCHAR        '.         
029000 01  SWITCHAR.                                                            
029100                                                                          
029200     03  UPPDATERA-SW        PIC X     VALUE 'N'.                         
029300       88  UPPDATERA                   VALUE 'J'.                         
029400                                                                          
029500     03  CORE-SW             PIC X     VALUE 'N'.                         
029600       88  CORE-OK                     VALUE 'J'.                         
029700                                                                          
030000                                                                          
030100     03  INDATA-SW           PIC X     VALUE 'J'.                         
030200       88  INDATA-OK                   VALUE 'J'.                         
030300                                                                          
030400     03  WS-IDTRANS          PIC X(4).                                    
030500       88  EGEN-BILD                   VALUE '6163'.                      
030600       88  GODK-BILD                   VALUE '6161' '6162' '6164'         
030700                                             '6165'.                      
030800                                                                          
030900     03  ARTC-UPPD-SW      PIC X     VALUE 'N'.                           
031000       88  ARTC-UPPD                 VALUE 'J'.                           
031100                                                                          
031400                                                                          
031500     03  TOM-MID-SW          PIC X     VALUE 'J'.                         
031600       88  TOM-MID                     VALUE 'J'.                         
031700                                                                          
031800     03  DISPATCH-SW         PIC X     VALUE 'N'.                         
031900       88  STARTA-DISPATCH             VALUE 'J'.                         
032000                                                                          
032100     03  PRINTER-SW          PIC X     VALUE 'J'.                         
032200       88  PRINTER-OK                  VALUE 'J'.                         
032300                                                                          
032400     03  ARTIKEL-SW          PIC X     VALUE 'J'.                         
032500       88  ARTIKEL-OK                  VALUE 'J'.                         
032600*    --- FÖR TEST AV BYTESNR                                              
032700*                                                                         
032800 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
032900 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
033000*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
033100                                                                          
033200*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
033300                                                                          
033400*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
033500                                                                          
033600                                                                          
033700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
033800 01  FILLER                  PIC X(16)  VALUE 'GEN-SUBPGM      '.         
033900 01  GENERELLA-SUBPROGRAM.                                                
034000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
034100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
034200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
034300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
034400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
034500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
034600*                                                                         
034700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
034800*   -COPY WMEDAREA                                                        
034900     EJECT                                                                
035000 01  FILLER                        PIC X(16)   VALUE 'INFO AREA'.         
035100 01  MESSAGE-CODES.                                                       
035200     03  INFO-TRYCK-PF23           PIC X(3)    VALUE '206'.               
035300     SKIP3                                                                
035400                                                                          
035500 01  FILLER                  PIC X(16)  VALUE 'W006PRT-AREA    '.         
035600                                                                          
035700*    --- PARAMETRAR TILL SUBPROGRAM W006PRT                               
035800*01 -COPY W006PRT                                                         
035900     EJECT                                                                
036000                                                                          
036100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
036200*01 -COPY WMSGINIT                                                        
036300     EJECT                                                                
036400 01  FILLER                  PIC X(16)  VALUE 'DECAREA    '.              
036500     SKIP3                                                                
036600     EJECT                                                                
036700 01  FILLER                  PIC X(16)  VALUE 'INDEX-FALT      '.         
036800 01  INDEX-FALT.                                                          
036900     03  TYP                 PIC S9(9) COMP SYNC    VALUE +1.             
037000     03  IX                  PIC S9(9) COMP SYNC    VALUE ZERO.           
037100     03  CD-IX               PIC S9(9) COMP SYNC    VALUE ZERO.           
037200                                                                          
037300 01  FILLER                  PIC X(16)  VALUE 'SPAR-FALT       '.         
037400 01  SPAR-FALT.                                                           
037500     03 SPAR-ADBUFFOMR       PIC 9(2) OCCURS 3.                           
037600     03 SPAR-ADBUFFGANG      PIC 9(2) OCCURS 3.                           
037700     03 SPAR-ADBUFFPL        PIC 9(5) OCCURS 3.                           
037800                                                                          
037900 01  FILLER                  PIC X(16)  VALUE 'FALT-AREA       '.         
038000 01  FALT-AREA.                                                           
038100     88  INTE-NAGON-IFYLLD          VALUE ALL 'I'.                        
038200         05  FALT-ADLAGOMR-CDC  PIC X.                                    
038300         05  FALT-ADGANG-CDC    PIC X.                                    
038400         05  FALT-ADPLATS-CDC   PIC X.                                    
038500         05  FALT-ADLAGOMR-SVS  PIC X.                                    
038600         05  FALT-ADGANG-SVS    PIC X.                                    
038700         05  FALT-ADPLATS-SVS   PIC X.                                    
038800         05  FALT-ADGANG-CD1    PIC X.                                    
038900         05  FALT-ADPLATS-CD1   PIC X.                                    
039000         05  FALT-ADGANG-CD2    PIC X.                                    
039100         05  FALT-ADPLATS-CD2   PIC X.                                    
039200         05  FALT-ADGANG-CD3    PIC X.                                    
039300         05  FALT-ADPLATS-CD3   PIC X.                                    
039400         05  FALT-ADGANG-CD4    PIC X.                                    
039500         05  FALT-ADPLATS-CD4   PIC X.                                    
039600*        05  FALT-VKART         PIC X.                                    
039700*        05  FALT-VLARTNTO      PIC X.                                    
039800*        05  FALT-KDVSOP        PIC X.                                    
039900         05  FALT-KDSPEEMB      PIC X.                                    
040000         05  FALT-IDARTNR-EMBQ3 PIC X.                                    
040100         05  FALT-IDARTNR-EMBQ4 PIC X.                                    
040200         05  FALT-FLEJBUFF      PIC X.                                    
040300         05  FALT-ADINLOMR-BOA  PIC X.                                    
040400         05  FALT-KVMAXPL       PIC X.                                    
040500                                                                          
040600 01  FILLER     REDEFINES FALT-AREA.                                      
040700     03  FALT     OCCURS 26.                                              
040800         05  FILLER           PIC X.                                      
040900     EJECT                                                                
041000                                                                          
041100 01  FILLER                   PIC X(16)  VALUE 'MID MID MID MID '.        
041200 01  FILLER                   PIC X(8)    VALUE 'MFS-WS  '.               
041300*01      MID -COPY W6I16301                                               
041400     EJECT                                                                
041500*01      -COPY WMSGAREA                                                   
041600     EJECT                                                                
041700*  03    W6O16301 -COPY W6O16301           -RED MSG-AREA.                 
041800     EJECT                                                                
041900*01      -COPY WMFSAREA                                                   
042000     EJECT                                                                
042100                                                                          
042200 01  FILLER                   PIC X(16)   VALUE 'KOM-IO-AREA'.            
042300     SKIP3                                                                
042400 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
042500 01  KOM-MSG-IO-AREA.                                                     
042600*03  -COPY WMSGKOM                                                        
042700     EJECT                                                                
042800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
042900 01  P-TO-P-SW.                                                           
043000     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
043100     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
043200     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
043300     02     P-TO-P-KDTRANS          PIC X(8).                             
043400     02     P-TO-P-IDTRANS          PIC X(4).                             
043500     02     P-TO-P-KDMFSFOR         PIC X(1).                             
043600     02     P-TO-P-DATA             PIC X(1000).                          
043700     EJECT                                                                
043800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
043900 01  P-TO-P-SW2.                                                          
044000     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
044100     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
044200     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
044300     02     P-TO-P2-KDTRANS          PIC X(8).                            
044400     02     P-TO-P2-IDTRANS          PIC X(4).                            
044500     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
044600     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
044700*****************************************************************         
044800     EJECT                                                                
044900                                                                          
045000 01      FILLER                  PIC X(24)   VALUE                        
045100                                 'MOD619A-MID-W6I19A01'.                  
045200     -COPY W6I19A01 -PRE MOD619A-                                         
045300     EJECT                                                                
045400 01      FILLER                  PIC X(24)   VALUE                        
045500                                 'MOD619B-MID-W6I19B01'.                  
045600     SKIP2                                                                
045700     -COPY W6I19B01 -PRE MOD619B-                                         
045800     EJECT                                                                
045900 01      FILLER                  PIC X(24)   VALUE                        
046000                                 'MOD4289-MID-W4I28901'.                  
046100     SKIP2                                                                
046200     -COPY W4I28901 -PRE MOD4289-                                         
046300     EJECT                                                                
046400***  ARBETSAREOR TILL IMS-SEKTIONERNA                                     
046500                                                                          
046600 01  IMS-WS.                                                              
046700     03  FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
046800     SKIP2                                                                
046900                                                                          
047000***  STATUSKOD FRÅN IMS                                                   
047100     03  STATUS-WS       PIC XX.                                          
047200         88  SEGMENT-FINNS               VALUE '  '.                      
047300         88  SEGMENT-SAKNAS              VALUE 'GE'.                      
047400         88  SEGMENT-FINNS-REDAN         VALUE 'II'.                      
047500     SKIP3                                                                
047600     03  GODK-STATUSKODER.                                                
047700         05  GODK-STATUS OCCURS 5    INDEXED BY STATUS-IX PIC XX.         
047800     SKIP3                                                                
047900                                                                          
048000 01  FILLER                  PIC X(16)  VALUE 'SSA1            '.         
048100 01  SSA1                    PIC X(128).                                  
048200 01  FILLER                  PIC X(16)  VALUE 'SSA2            '.         
048300 01  SSA2                    PIC X(64).                                   
048400 01  FILLER                  PIC X(16)  VALUE 'SSA3            '.         
048500 01  SSA3                    PIC X(32).                                   
048600     EJECT                                                                
048700***  IMS FUNKTIONSKODER                                                   
048800*01      -COPY W0003                                                      
048900     EJECT                                                                
049000***  DLI INPUT-OUTPUT AREA                                                
049100 01  DLI-IO-AREA.                                                         
049200     03 FILLER                  PIC X(16)   VALUE 'IO-AREA-1'.            
049300     03 IO-AREA-1               PIC X(152)  VALUE SPACE.                  
049400     SKIP3                                                                
049500*    03  W6PLAA11 -COPY W6GX6006 -PRE PLAA- -RED IO-AREA-1.               
049600     EJECT                                                                
049700*    03  WLARTD01 -COPY WDD801 -PRE ARTD-    -RED IO-AREA-1.              
049800     EJECT                                                                
049900*    03  WLARTD11 -COPY WDD811 -PRE ARTD-    -RED IO-AREA-1.              
050000     EJECT                                                                
050100     03     FILLER              PIC X(16)   VALUE 'IO-AREA-2'.            
050200     03     IO-AREA-2           PIC X(900)  VALUE SPACE.                  
050300     SKIP3                                                                
050400*    03  WLARTC01 -COPY WDK601             -RED IO-AREA-2.                
050500     EJECT                                                                
050600*    03  WLARTC11 -COPY WDK611             -RED IO-AREA-2.                
050700     EJECT                                                                
050800                                                                          
050900 01  FILLER                 PIC X(16) VALUE 'DLI-IO-WDK712'.              
051000 01  DLI-IO-AREA-WDK712.                                                  
051100*    03  -COPY WDK712                                                     
051200     EJECT                                                                
051300                                                                          
051400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLLOCB01'.        
051500 01  DLI-IO-WLLOCB01.                                                     
051600*    03  -COPY WDJ901    -PRE LOCB-                                       
051700     EJECT                                                                
051800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLLOCB11'.        
051900 01  DLI-IO-WLLOCB11.                                                     
052000*    03  -COPY WDJ911    -PRE LOCB-                                       
052100                                                                          
052200     SKIP3                                                                
052300 LINKAGE SECTION.                                                         
052400*01      -COPY W0009     -PRE MSG-                                        
052500     EJECT                                                                
052600*01      -COPY W0009     -PRE DISP-                                       
052700     EJECT                                                                
052800*01      -COPY W0009     -PRE ALT-                                        
052900     EJECT                                                                
053000*01      -COPY W0009     -PRE 4289-                                       
053100     EJECT                                                                
053200*01      -COPY W0008     -PRE USEA-                                       
053300      05 FILLER          PIC X.                                           
053400     EJECT                                                                
053500*01      -COPY W0008     -PRE ARTC-                                       
053600      05 FILLER          PIC X.                                           
053700     EJECT                                                                
053800*01      -COPY W0008     -PRE WDK6-                                       
053900      05 FILLER          PIC X.                                           
054000     EJECT                                                                
054100*01      -COPY W0008     -PRE WDK7-                                       
054200      05 FILLER          PIC X.                                           
054300     EJECT                                                                
054400*01      -COPY W0008     -PRE ARTD-                                       
054500      05 FILLER          PIC X.                                           
054600                                                                          
054700*01      -COPY W0008     -PRE PLAA-                                       
054800      05 FILLER          PIC X.                                           
054900                                                                          
055000*    PCB'ER FÖR SUBPGM                                                    
055100                                                                          
055200 01 KOM-KOMA-PCB         PIC X.                                           
055300                                                                          
055400*    PCB FÖR PLATSREGISTRET                                               
055500                                                                          
055600*01      -COPY W0008     -PRE LOCB-                                       
055700      05 FILLER          PIC X.                                           
055800     EJECT                                                                
055900                                                                          
056000 PROCEDURE DIVISION USING MSG-PCB  DISP-PCB ALT-PCB  4289-PCB             
056100                          USEA-PCB ARTC-PCB WDK6-PCB WDK7-PCB             
056200                          ARTD-PCB PLAA-PCB KOM-KOMA-PCB                  
056300                          LOCB-PCB.                                       
056400                                                                          
056500      ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB ALT-PCB  4289-PCB           
056600                          USEA-PCB ARTC-PCB WDK6-PCB WDK7-PCB             
056700                          ARTD-PCB PLAA-PCB KOM-KOMA-PCB                  
056800                          LOCB-PCB.                                       
056900 STYR SECTION.                                                            
057000     PERFORM IMS-GET-MSG                                                  
057100     IF SEGMENT-FINNS                                                     
057200       PERFORM A-INIT-SPARA-INPUT                                         
057300       IF IDARTNR-WS NUMERIC                                              
057400         MOVE IDARTNR-WS TO W-IDARTNR                                     
057500                            TEST-IDARTNR                                  
057600         PERFORM B-LAES-BEHANDLA                                          
057700         PERFORM C-KONTROLLERA-MIDEN                                      
057800         IF TOM-MID                                                       
057900           CONTINUE                                                       
058000         ELSE                                                             
058100           IF MFS-QUERY   AND EGEN-BILD                                   
058200             PERFORM MFS-ROER-EJ-BILD                                     
058300             MOVE RETT-2 (TYP) TO MOD-TEMFSFEL                            
058400           END-IF                                                         
058500         END-IF                                                           
058600         IF UPPDATERA AND EGEN-BILD AND INDATA-OK AND                     
058700           (MFS-UPDATE OR MFS-UPD-V)                                      
058800           PERFORM D-KONTROLLERA-DATA                                     
058900           IF INTE-NAGON-IFYLLD OR ADLAGOMR-FEL                           
059000             IF ADLAGOMR-FEL                                              
059100              MOVE FEL-11(TYP)  TO MOD-TEMFSFEL                           
059200              PERFORM S03-BACKA-MOD                                       
059300             ELSE                                                         
059400              MOVE RETT-3 (TYP) TO MOD-TEMFSFEL                           
059500             END-IF                                                       
059600           ELSE                                                           
059700             IF MID-ADLAGOMR-CDC = ALL '0'                                
059800             OR MID-ADPLATS-CDC = ALL '0'                                 
059900*            OR MID-KVMAXPL     = ALL '0'                                 
060000*            OR MID-ADLAGOMR-SVS = ALL '0'                                
060100*            OR MID-ADPLATS-SVS = ALL '0'                                 
060200*            OR MID-VKART = ALL '0'                                       
060300*            OR MID-VLARTNTO = ALL '0'                                    
060400               PERFORM F-KONTROLLERA-PLATS                                
060500             END-IF                                                       
060600             INSPECT FALT-AREA TALLYING  COUNTR FOR ALL 'F'               
060700             IF COUNTR = 0                                                
060800               PERFORM E-UPPDATERA-BASER                                  
060900             ELSE                                                         
061000               PERFORM S01-NAGON-FEL                                      
061100             END-IF                                                       
061200           END-IF                                                         
061300         ELSE                                                             
061400           IF MFS-PRINT AND EGEN-BILD                                     
061500             PERFORM G-SKRIV-PLATSSAETTARLISTA                            
061600           END-IF                                                         
061700         END-IF                                                           
061800       ELSE                                                               
061900         IF GODK-BILD                                                     
062000           PERFORM MFS-RENSA-BILDEN                                       
062100           MOVE FEL-1 (TYP) TO MOD-TEMFSFEL                               
062200         END-IF                                                           
062300       END-IF                                                             
062400       MOVE MAX-MODLAENGD TO MSG-KVLL                                     
062500       PERFORM IMS-ISRT-MSG                                               
062600     END-IF                                                               
062700     MOVE ZERO TO RETURN-CODE                                             
062800     GOBACK                                                               
062900     .                                                                    
063000     EJECT                                                                
063100                                                                          
063200 A-INIT-SPARA-INPUT SECTION.                                              
063300                                                                          
063400     ACCEPT DAGENS-DATUM       FROM DATE                                  
063500     ACCEPT DAGENS-TID         FROM TIME                                  
063600     MOVE NEJ TO DISPATCH-SW                                              
063700     MOVE NEJ TO CORE-SW                                                  
063900     MOVE ALL '+'              TO MOD619B-MID-W6I19B01                    
064000                                  MOD4289-MID-W4I28901                    
064100     MOVE JA  TO UPPDATERA-SW                                             
064200                                                                          
064300     IF MSG-DUBBLA-TRANSKODER                                             
064400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16301                 
064500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
064600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
064700     ELSE                                                                 
064800       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W6I16301                   
064900       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
065000       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
065100     END-IF                                                               
065200                                                                          
065300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
065400     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
065500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
065600                                                                          
065700     MOVE LOW-VALUE       TO MSG-AREA                                     
065800     MOVE 'W6O163N1'      TO MFS-IDMOD                                    
065900     MOVE '6163'          TO MOD-IDTRANS                                  
066000     MOVE '-'             TO MOD-STRECK                                   
066100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
066200                             MOD-TEMFSFEL                                 
066300                             MOD-TEMFSINF                                 
066400                             MOD-ADBUFFOMR-OLD(1)                         
066500                             MOD-ADBUFFGANG-OLD(1)                        
066600                             MOD-ADBUFFPL-OLD(1)                          
066700                                                                          
066800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
066900     MOVE '001'             TO MSGI-KDCALL                                
067000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
067100     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
067200     MOVE '6163'                 TO MSGI-IDTRANS                          
067300                                                                          
067400     IF MFS-IDTRANS = '6163'                                              
067500       OR (MID-IDARTNR-IN NUMERIC                                         
067600       AND MID-IDARTNR-IN > ZERO)                                         
067700       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
067800     END-IF                                                               
067900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
068000                                                                          
068100     IF MSGI-IDLAND-SPR = 'GB'                                            
068200       MOVE +2 TO TYP                                                     
068300     ELSE                                                                 
068400       MOVE +1 TO TYP                                                     
068500     END-IF                                                               
068600                                                                          
068700     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
068800     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
068900                                                                          
069000     IF MID-IDARTNR-IN = ALL '+'                                          
069100       CONTINUE                                                           
069200     ELSE                                                                 
069300       MOVE NEJ TO UPPDATERA-SW                                           
069400     END-IF                                                               
069500                                                                          
069600     IF (MFS-IDTRANS NOT = '6163')                                        
069700       MOVE NEJ TO UPPDATERA-SW                                           
069800     END-IF                                                               
069900                                                                          
070000     IF UPPDATERA                                                         
070100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                           
070200     ELSE                                                                 
070300       MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                  
070400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
070500     END-IF                                                               
070600                                                                          
070700     IF GODK-BILD OR IDARTNR-WS NUMERIC                                   
070800       CONTINUE                                                           
070900     ELSE                                                                 
071000       PERFORM MFS-RENSA-NYCKLAR                                          
071100       PERFORM MFS-RENSA-BILDEN                                           
071200     END-IF                                                               
071300     EJECT                                                                
071400     .                                                                    
071500                                                                          
071600 B-LAES-BEHANDLA         SECTION.                                         
071700                                                                          
071800***  ARTIKELINFORMATION                                                   
071900     MOVE JA                   TO ARTIKEL-SW                              
072000     PERFORM IMS-GU-K601                                                  
072100     IF SEGMENT-FINNS                                                     
072200       MOVE ART-REKSIFFR       TO MOD-REKSIFFR                            
072300                                                                          
072400       IF ART-KDERS-UTG > ZERO                                            
072500         MOVE FEL-3 (TYP)      TO MOD-TEMFSFEL                            
072600       END-IF                                                             
072700                                                                          
072800       PERFORM IMS-GHNP-K611                                              
072900       IF SEGMENT-FINNS                                                   
073000         MOVE CLAG-VKART              TO MOD-VKART-OLD                    
073100         MOVE CLAG-VLARTNTO           TO MOD-VLARTNTO-OLD                 
073200         MOVE CLAG-KDVSOP             TO MOD-KDVSOP-OLD                   
073300         MOVE CLAG-IDARTNR-EMBQ3 TO MOD-IDARTNR-EMBQ3-OLD                 
073400         MOVE CLAG-IDARTNR-EMBQ4 TO MOD-IDARTNR-EMBQ4-OLD                 
073500                                                                          
073600         IF CLAG-FLEJBUFF = JA AND MSGI-IDLAND-SPR = 'GB'                 
073700           MOVE YES                   TO MOD-FLEJBUFF-OLD                 
073800         ELSE                                                             
073900           MOVE CLAG-FLEJBUFF         TO MOD-FLEJBUFF-OLD                 
074000         END-IF                                                           
074100                                                                          
074200         MOVE CLAG-ADINLOMR-BOA       TO MOD-ADINLOMR-BOA-OLD             
074300                                                                          
074400         MOVE MFS-RENSA-FAELT         TO MOD-NEW (18)                     
074500                                         MOD-NEW (19)                     
074600                                         MOD-NEW (20)                     
074700                                         MOD-NEW (22)                     
074800                                         MOD-NEW (23)                     
074900                                         MOD-NEW (23)                     
075000                                         MOD-NEW (24)                     
075100                                         MOD-NEW (25)                     
075200       ELSE                                                               
075300         MOVE FEL-2 (TYP)       TO MOD-TEMFSFEL                           
075400         MOVE NEJ               TO INDATA-SW                              
075500       END-IF                                                             
075600***   BEHÖRIGHETSKONTROLL                                                 
075700       IF SEGMENT-FINNS                                                   
075800         IF MFS-UPDATE AND CLAG-ADLAGOMR = 91                             
075900           MOVE INFO-TRYCK-PF23 TO MED-IDMFSFEL                           
076000           CALL WMEDKONV USING MED-WMEDAREA                               
076100           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
076200           MOVE NEJ             TO INDATA-SW                              
076300         END-IF                                                           
076400       END-IF                                                             
076500***   FÖRRÅDS-INFO                                                        
076600       IF SEGMENT-FINNS                                                   
076700         MOVE CLAG-ADLAGOMR TO MOD-ADLAGOMR-CDC-OLD                       
076800                                    W-ADLAGOMR-WDJ9-CDC                   
076900         MOVE CLAG-ADGANG        TO MOD-ADGANG-CDC-OLD                    
077000                                    W-ADGANG-WDJ9-CDC                     
077100         MOVE CLAG-ADPLATS       TO MOD-ADPLATS-CDC-OLD                   
077200                                    W-ADPLATS-WDJ9-CDC                    
077300         MOVE CLAG-KVMAXPL       TO MOD-KVMAXPL-OLD                       
077400                                                                          
077500*        MOVE CLAG-ADLAGOMR-SVS                                           
077600*                                TO MOD-ADLAGOMR-SVS-OLD                  
077700*                                   W-ADLAGOMR-WDJ9-SVS                   
077800*        MOVE CLAG-ADGANG-SVS                                             
077900*                                TO MOD-ADGANG-SVS-OLD                    
078000*                                   W-ADGANG-WDJ9-SVS                     
078100*        MOVE CLAG-ADPLATS-SVS                                            
078200*                                TO MOD-ADPLATS-SVS-OLD                   
078300*                                   W-ADPLATS-WDJ9-SVS                    
078400         MOVE CLAG-KVQPACK-3                                              
078500                                 TO MOD-KVQPACK-3                         
078600         MOVE CLAG-KVREFPKT-PLOCK                                         
078700                                 TO MOD-KVREFPKT-PLOCK                    
078800                                                                          
078900         MOVE 1 TO CD-IX                                                  
079000         PERFORM UNTIL CD-IX > 4                                          
079100           MOVE CLAG-ADLAGOMR-CD(CD-IX)                                   
079200                                   TO MOD-ADLAGOMR-CD-OLD(CD-IX)          
079300           MOVE CLAG-ADGANG-CD(CD-IX)                                     
079400                                   TO MOD-ADGANG-CD-OLD(CD-IX)            
079500           MOVE CLAG-ADPLATS-CD(CD-IX)                                    
079600                                   TO MOD-ADPLATS-CD-OLD(CD-IX)           
079700           ADD 1 TO CD-IX                                                 
079800         END-PERFORM                                                      
079900                                                                          
080000         MOVE CLAG-KDSPEEMB TO MOD-KDSPEEMB-OLD                           
080100                                                                          
080200         MOVE MFS-RENSA-FAELT    TO MOD-NEW (1)                           
080300                                    MOD-NEW (2)                           
080400                                    MOD-NEW (3)                           
080500                                    MOD-NEW (4)                           
080600                                    MOD-NEW (5)                           
080700                                    MOD-NEW (6)                           
080800                                    MOD-NEW (7)                           
080900                                    MOD-NEW (8)                           
081000                                    MOD-NEW (9)                           
081100                                    MOD-NEW (10)                          
081200                                    MOD-NEW (11)                          
081300                                    MOD-NEW (12)                          
081400                                    MOD-NEW (13)                          
081500                                    MOD-NEW (14)                          
081600                                    MOD-NEW (15)                          
081700                                    MOD-NEW (16)                          
081800                                    MOD-NEW (17)                          
081900                                    MOD-NEW (21)                          
082000                                    MOD-NEW (26)                          
082100         PERFORM BA-LAES-SALDO                                            
082200         MOVE SPACE TO STATUS-WS                                          
082300       ELSE                                                               
082400         MOVE FEL-4 (TYP)  TO  MOD-TEMFSFEL                               
082500         MOVE NEJ          TO  ARTIKEL-SW                                 
082600       END-IF                                                             
082700     ELSE                                                                 
082800       MOVE   MFS-RENSA-FAELT  TO MOD-VKART-OLD                           
082900                                  MOD-VLARTNTO-OLD                        
083000                                  MOD-KDVSOP-OLD                          
083100                                  MOD-KDSPEEMB-OLD                        
083200                                  MOD-ADLAGOMR-CDC-OLD                    
083300                                  MOD-ADGANG-CDC-OLD                      
083400                                  MOD-ADPLATS-CDC-OLD                     
083500                                  MOD-KVMAXPL-OLD                         
083600*                                 MOD-ADLAGOMR-SVS-OLD                    
083700*                                 MOD-ADGANG-SVS-OLD                      
083800*                                 MOD-ADPLATS-SVS-OLD                     
083900                                  MOD-KVQPACK-3                           
084000                                  MOD-KVREFPKT-PLOCK                      
084100                                  MOD-ADLAGOMR-CD-OLD(1)                  
084200                                  MOD-ADGANG-CD-OLD(1)                    
084300                                  MOD-ADPLATS-CD-OLD(1)                   
084400                                  MOD-ADLAGOMR-CD-OLD(2)                  
084500                                  MOD-ADGANG-CD-OLD(2)                    
084600                                  MOD-ADPLATS-CD-OLD(2)                   
084700                                  MOD-ADLAGOMR-CD-OLD(3)                  
084800                                  MOD-ADGANG-CD-OLD(3)                    
084900                                  MOD-ADPLATS-CD-OLD(3)                   
085000                                  MOD-ADLAGOMR-CD-OLD(4)                  
085100                                  MOD-ADGANG-CD-OLD(4)                    
085200                                  MOD-ADPLATS-CD-OLD(4)                   
085300                                  MOD-ADBUFFOMR-OLD  (1)                  
085400                                  MOD-ADBUFFGANG-OLD (1)                  
085500                                  MOD-ADBUFFPL-OLD   (1)                  
085600                                  MOD-ADBUFFOMR-OLD  (2)                  
085700                                  MOD-ADBUFFGANG-OLD (2)                  
085800                                  MOD-ADBUFFPL-OLD   (2)                  
085900                                  MOD-ADBUFFOMR-OLD  (3)                  
086000                                  MOD-ADBUFFGANG-OLD (3)                  
086100                                  MOD-ADBUFFPL-OLD   (3)                  
086200                                  MOD-IDARTNR-EMBQ3-OLD                   
086300                                  MOD-IDARTNR-EMBQ4-OLD                   
086400                                  MOD-FLEJBUFF-OLD                        
086500                                  MOD-ADINLOMR-BOA-OLD                    
086600                                                                          
086700       MOVE +1 TO IX                                                      
086800       PERFORM UNTIL IX > RAD-MAX                                         
086900           MOVE MFS-STAENG-FAELT TO MOD-NEW-ATTR (IX)                     
087000           MOVE MFS-RENSA-FAELT  TO MOD-NEW (IX)                          
087100           ADD +1 TO IX                                                   
087200       END-PERFORM                                                        
087300       MOVE FEL-2 (TYP) TO MOD-TEMFSFEL                                   
087400       MOVE NEJ         TO ARTIKEL-SW                                     
087500                           INDATA-SW                                      
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 BA-LAES-SALDO SECTION.                                                   
088100                                                                          
088200     PERFORM IMS-GU-ARTD01                                                
088300     IF SEGMENT-FINNS                                                     
088400       PERFORM IMS-GNP-ARTD11                                             
088500       MOVE +1  TO IX-BUFF                                                
088600       PERFORM  UNTIL IX-BUFF > 3 OR SEGMENT-SAKNAS                       
088700         IF SEGMENT-FINNS                                                 
088800           MOVE ARTD-SALDO-ADBUFFOMR                                      
088900                                   TO MOD-ADBUFFOMR-OLD (IX-BUFF)         
089000           MOVE ARTD-SALDO-ADBUFFGANG                                     
089100                                   TO MOD-ADBUFFGANG-OLD(IX-BUFF)         
089200           MOVE ARTD-SALDO-ADBUFFPL                                       
089300                                   TO MOD-ADBUFFPL-OLD  (IX-BUFF)         
089400           ADD +1 TO IX-BUFF                                              
089500           PERFORM IMS-GNP-ARTD11                                         
089600         ELSE                                                             
089700           MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR-OLD (IX-BUFF)            
089800                                   MOD-ADBUFFGANG-OLD(IX-BUFF)            
089900                                   MOD-ADBUFFPL-OLD  (IX-BUFF)            
090000           ADD +1               TO IX-BUFF                                
090100         END-IF                                                           
090200       END-PERFORM                                                        
090300     ELSE                                                                 
090400       IF NOT ENGLISH-TEXT                                                
090500         MOVE 'ARTIKELN FINNS EJ I BUFFERTREGISTRET'                      
090600                                   TO MOD-TEMFSFEL                        
090700       END-IF                                                             
090800     END-IF                                                               
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200 C-KONTROLLERA-MIDEN  SECTION.                                            
091300                                                                          
091400     IF MID-ADLAGOMR-CDC  NOT = ALL '+'       OR                          
091500        MID-ADGANG-CDC    NOT = ALL '+'       OR                          
091600        MID-ADPLATS-CDC   NOT = ALL '+'       OR                          
091700        MID-KVMAXPL       NOT = ALL '+'       OR                          
091800*       MID-ADLAGOMR-SVS  NOT = ALL '+'       OR                          
091900*       MID-ADGANG-SVS    NOT = ALL '+'       OR                          
092000*       MID-ADPLATS-SVS   NOT = ALL '+'       OR                          
092100        MID-ADGANG-CD(1)  NOT = ALL '+'       OR                          
092200        MID-ADPLATS-CD(1) NOT = ALL '+'       OR                          
092300        MID-ADGANG-CD(2)  NOT = ALL '+'       OR                          
092400        MID-ADPLATS-CD(2) NOT = ALL '+'       OR                          
092500        MID-ADGANG-CD(3)  NOT = ALL '+'       OR                          
092600        MID-ADPLATS-CD(3) NOT = ALL '+'       OR                          
092700        MID-ADGANG-CD(4)  NOT = ALL '+'       OR                          
092800        MID-ADPLATS-CD(4) NOT = ALL '+'       OR                          
093200        MID-KDSPEEMB      NOT = ALL '+'       OR                          
093300        MID-IDARTNR-EMBQ3 NOT = ALL '+'       OR                          
093400        MID-IDARTNR-EMBQ4 NOT = ALL '+'       OR                          
093500        MID-FLEJBUFF      NOT = ALL '+'       OR                          
093600        MID-ADINLOMR-BOA  NOT = ALL '+'                                   
093700                                                                          
093800        MOVE NEJ TO TOM-MID-SW                                            
093900     END-IF                                                               
094000     .                                                                    
094100     EJECT                                                                
094200                                                                          
094300 D-KONTROLLERA-DATA SECTION.                                              
094400                                                                          
094500     MOVE NEJ TO ARTC-UPPD-SW                                             
094600***  SALDOSEG-UPPD-SW  ** ANVÄNDS EJ  **                                  
094700                                                                          
094800     IF MID-ADLAGOMR-CDC = ALL '+'                                        
094900       MOVE    INGET-IFYLLT     TO FALT-ADLAGOMR-CDC                      
095000       MOVE MFS-ROER-EJ-FAELT   TO MOD-ADLAGOMR-CDC-OLD                   
095100     ELSE                                                                 
095200       IF MID-ADLAGOMR-CDC NUMERIC                                        
095300         MOVE MID-ADLAGOMR-CDC       TO W-ADINLOMR                        
095400                                        WX-ADLAGOMR                       
095500         MOVE CLAG-ADLAGOMR          TO WY-ADLAGOMR                       
095600         PERFORM S02-ADLAGOMR-KOLL                                        
095700         PERFORM IMS-GU-PLAA-6006                                         
095800         IF SEGMENT-FINNS                                                 
095900           MOVE       RETT      TO FALT-ADLAGOMR-CDC                      
096000           MOVE MFS-ADD-LYS-UPP-FAELT                                     
096100                                TO MOD-ADLAGOMR-CDC-OLD-ATTR              
096200           MOVE     MID-ADLAGOMR-CDC                                      
096300                                TO MOD-ADLAGOMR-CDC-OLD                   
096510           IF  MID-ADLAGOMR-CDC = '10'  OR '11' OR '20' OR '21' OR        
096520                          '22' OR '30'  OR '31' OR '32' OR '33' OR        
096530                          '34' OR '35'  OR '36' OR '37' OR '91'           
096600              IF MID-KVMAXPL = ALL '+'                                    
096700                 IF CLAG-KVMAXPL = ZERO                                   
096800                    MOVE FEL           TO FALT-ADLAGOMR-CDC               
096900                                          FALT-KVMAXPL                    
097000                    MOVE FEL-5 (TYP)   TO MOD-TEMFSFEL                    
097100                 ELSE                                                     
097200                    MOVE JA            TO ARTC-UPPD-SW                    
097300                 END-IF                                                   
097400              ELSE                                                        
097500                 IF MID-KVMAXPL = ZERO                                    
097600                    MOVE FEL           TO FALT-ADLAGOMR-CDC               
097700                                          FALT-KVMAXPL                    
097800                    MOVE FEL-5 (TYP)   TO MOD-TEMFSFEL                    
097900                 ELSE                                                     
098000                    MOVE JA            TO ARTC-UPPD-SW                    
098100                 END-IF                                                   
098200              END-IF                                                      
098300           ELSE                                                           
098400              IF MID-KVMAXPL = ALL '+' AND CLAG-KVMAXPL = 0               
098500              OR MID-KVMAXPL = ZERO                                       
098600                 MOVE  JA              TO ARTC-UPPD-SW                    
098700              ELSE                                                        
098800                 MOVE FEL              TO FALT-ADLAGOMR-CDC               
098900                                          FALT-KVMAXPL                    
099000                 MOVE FEL-5 (TYP)      TO MOD-TEMFSFEL                    
099100              END-IF                                                      
099200           END-IF                                                         
099300         ELSE                                                             
099400           MOVE       FEL              TO FALT-ADLAGOMR-CDC               
099500           MOVE FEL-10 (TYP)           TO MOD-TEMFSFEL                    
099600         END-IF                                                           
099700       ELSE                                                               
099800         MOVE       FEL              TO FALT-ADLAGOMR-CDC                 
099900         MOVE FEL-5 (TYP)            TO MOD-TEMFSFEL                      
100000       END-IF                                                             
100100     END-IF                                                               
100200                                                                          
100300     IF MID-ADGANG-CDC = ALL '+'                                          
100400       MOVE INGET-IFYLLT            TO FALT-ADGANG-CDC                    
100500       MOVE MFS-ROER-EJ-FAELT       TO MOD-ADGANG-CDC-OLD                 
100600     ELSE                                                                 
100700       IF MID-ADGANG-CDC NUMERIC                                          
100800         IF MID-ADLAGOMR-CDC NUMERIC                                      
100900           MOVE MID-ADLAGOMR-CDC     TO WX-ADLAGOMR                       
101000         ELSE                                                             
101100           MOVE ZERO                 TO WX-ADLAGOMR                       
101200         END-IF                                                           
101300         MOVE CLAG-ADLAGOMR          TO WY-ADLAGOMR                       
101400         PERFORM S02-ADLAGOMR-KOLL                                        
101500         MOVE RETT                  TO FALT-ADGANG-CDC                    
101600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-CDC-OLD-ATTR            
101700         MOVE MID-ADGANG-CDC        TO MOD-ADGANG-CDC-OLD                 
101800         MOVE JA                    TO ARTC-UPPD-SW                       
101900       ELSE                                                               
102000         MOVE FEL                   TO FALT-ADGANG-CDC                    
102100         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
102200       END-IF                                                             
102300     END-IF                                                               
102400                                                                          
102500     IF MID-ADPLATS-CDC = ALL '+'                                         
102600       MOVE INGET-IFYLLT            TO FALT-ADPLATS-CDC                   
102700       MOVE MFS-ROER-EJ-FAELT       TO MOD-ADPLATS-CDC-OLD                
102800     ELSE                                                                 
102900       INSPECT MID-ADPLATS-CDC REPLACING LEADING SPACE BY ZERO            
103000       IF MID-ADPLATS-CDC NUMERIC                                         
103100         IF MID-ADLAGOMR-CDC NUMERIC                                      
103200           MOVE MID-ADLAGOMR-CDC     TO WX-ADLAGOMR                       
103300         ELSE                                                             
103400           MOVE ZERO                 TO WX-ADLAGOMR                       
103500         END-IF                                                           
103600         MOVE CLAG-ADLAGOMR          TO WY-ADLAGOMR                       
103700         PERFORM S02-ADLAGOMR-KOLL                                        
103800         MOVE RETT                  TO FALT-ADPLATS-CDC                   
103900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-CDC-OLD-ATTR           
104000         MOVE MID-ADPLATS-CDC       TO MOD-ADPLATS-CDC-OLD                
104100         MOVE JA                    TO ARTC-UPPD-SW                       
104200       ELSE                                                               
104300         MOVE FEL                   TO FALT-ADPLATS-CDC                   
104400         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
104500       END-IF                                                             
104600     END-IF                                                               
104700                                                                          
104800     IF MID-KVMAXPL = ALL '+'                                             
104900       MOVE INGET-IFYLLT            TO FALT-KVMAXPL                       
105000       MOVE MFS-ROER-EJ-FAELT       TO MOD-KVMAXPL-OLD                    
105100     ELSE                                                                 
105200       INSPECT MID-KVMAXPL REPLACING LEADING SPACE BY ZERO                
105300       IF MID-KVMAXPL NUMERIC                                             
105400         IF MID-ADLAGOMR-CDC = ALL '+'                                    
105500           IF (CLAG-ADLAGOMR       = 10 OR 11 OR 20 OR 21 OR              
105600                             22 OR 30 OR 31 OR 32 OR 33 OR                
105601                             34 OR 35 OR 36 OR 37 OR 91)                  
105700           AND MID-KVMAXPL NOT = ZERO                                     
105800                                                                          
105900              MOVE RETT             TO FALT-KVMAXPL                       
106000              MOVE JA               TO ARTC-UPPD-SW                       
106100              MOVE MFS-ADD-LYS-UPP-FAELT                                  
106200                                    TO MOD-KVMAXPL-OLD-ATTR               
106300              MOVE MID-KVMAXPL      TO MOD-KVMAXPL-OLD                    
106400           ELSE                                                           
106500              MOVE FEL              TO FALT-KVMAXPL                       
106600              MOVE FEL-5 (TYP)      TO MOD-TEMFSFEL                       
106700           END-IF                                                         
106800         ELSE                                                             
107010           IF  MID-ADLAGOMR-CDC = '10'  OR '11' OR '20' OR '21' OR        
107020                          '22' OR '30'  OR '31' OR '32' OR '33' OR        
107030                          '34' OR '35'  OR '36' OR '37' OR '91'           
107100              MOVE RETT             TO FALT-KVMAXPL                       
107200              MOVE JA               TO ARTC-UPPD-SW                       
107300              MOVE MFS-ADD-LYS-UPP-FAELT                                  
107400                                    TO MOD-KVMAXPL-OLD-ATTR               
107500              MOVE MID-KVMAXPL      TO MOD-KVMAXPL-OLD                    
107600           ELSE                                                           
107700              IF MID-KVMAXPL = ZERO                                       
107800                 MOVE RETT          TO FALT-KVMAXPL                       
107900                 MOVE JA            TO ARTC-UPPD-SW                       
108000                 MOVE MFS-ADD-LYS-UPP-FAELT                               
108100                                    TO MOD-KVMAXPL-OLD-ATTR               
108200                 MOVE MID-KVMAXPL   TO MOD-KVMAXPL-OLD                    
108300              ELSE                                                        
108400                 MOVE FEL           TO FALT-KVMAXPL                       
108500                 MOVE FEL-5 (TYP)   TO MOD-TEMFSFEL                       
108600              END-IF                                                      
108700           END-IF                                                         
108800         END-IF                                                           
108900       ELSE                                                               
109000         MOVE FEL                   TO FALT-KVMAXPL                       
109100         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
109200       END-IF                                                             
109300     END-IF                                                               
109400                                                                          
109500*    IF MID-ADLAGOMR-SVS = ALL '+'                                        
109600*      MOVE    INGET-IFYLLT     TO FALT-ADLAGOMR-SVS                      
109700*      MOVE MFS-ROER-EJ-FAELT   TO MOD-ADLAGOMR-SVS-OLD                   
109800*    ELSE                                                                 
109900*      IF MID-ADLAGOMR-SVS NUMERIC                                        
110000*        MOVE MID-ADLAGOMR-SVS       TO W-ADINLOMR                        
110100*                                       WX-ADLAGOMR                       
110200*        MOVE CLAG-ADLAGOMR-SVS        TO WY-ADLAGOMR                     
110300*        PERFORM S02-ADLAGOMR-KOLL                                        
110400*        PERFORM IMS-GU-PLAA-6006                                         
110500*        IF SEGMENT-FINNS                                                 
110600*          MOVE       RETT      TO FALT-ADLAGOMR-SVS                      
110700*          MOVE MFS-ADD-LYS-UPP-FAELT                                     
110800*                               TO MOD-ADLAGOMR-SVS-OLD-ATTR              
110900*          MOVE     MID-ADLAGOMR-SVS                                      
111000*                               TO MOD-ADLAGOMR-SVS-OLD                   
111100*          MOVE        JA              TO ARTC-UPPD-SW                    
111200*        ELSE                                                             
111300*          MOVE       FEL              TO FALT-ADLAGOMR-SVS               
111400*          MOVE FEL-10 (TYP)           TO MOD-TEMFSFEL                    
111500*        END-IF                                                           
111600*      ELSE                                                               
111700*        MOVE       FEL              TO FALT-ADLAGOMR-SVS                 
111800*        MOVE FEL-5 (TYP)            TO MOD-TEMFSFEL                      
111900*      END-IF                                                             
112000*    END-IF                                                               
112100*                                                                         
112200*    IF MID-ADGANG-SVS = ALL '+'                                          
112300*      MOVE INGET-IFYLLT            TO FALT-ADGANG-SVS                    
112400*      MOVE MFS-ROER-EJ-FAELT       TO MOD-ADGANG-SVS-OLD                 
112500*    ELSE                                                                 
112600*      IF MID-ADGANG-SVS NUMERIC                                          
112700*        IF MID-ADLAGOMR-SVS NUMERIC                                      
112800*          MOVE MID-ADLAGOMR-SVS     TO WX-ADLAGOMR                       
112900*        ELSE                                                             
113000*          MOVE ZERO                 TO WX-ADLAGOMR                       
113100*        END-IF                                                           
113200*        MOVE CLAG-ADLAGOMR-SVS TO WY-ADLAGOMR                            
113300*        PERFORM S02-ADLAGOMR-KOLL                                        
113400*        MOVE RETT                  TO FALT-ADGANG-SVS                    
113500*        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-SVS-OLD-ATTR            
113600*        MOVE MID-ADGANG-SVS        TO MOD-ADGANG-SVS-OLD                 
113700*        MOVE JA                    TO ARTC-UPPD-SW                       
113800*      ELSE                                                               
113900*        MOVE FEL                   TO FALT-ADGANG-SVS                    
114000*        MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
114100*      END-IF                                                             
114200*    END-IF                                                               
114300*                                                                         
114400*    IF MID-ADPLATS-SVS = ALL '+'                                         
114500*      MOVE INGET-IFYLLT            TO FALT-ADPLATS-SVS                   
114600*      MOVE MFS-ROER-EJ-FAELT       TO MOD-ADPLATS-SVS-OLD                
114700*    ELSE                                                                 
114800*      INSPECT MID-ADPLATS-SVS REPLACING LEADING SPACE BY ZERO            
114900*      IF MID-ADPLATS-SVS NUMERIC                                         
115000*        IF MID-ADLAGOMR-SVS NUMERIC                                      
115100*          MOVE MID-ADLAGOMR-SVS     TO WX-ADLAGOMR                       
115200*        ELSE                                                             
115300*          MOVE ZERO                 TO WX-ADLAGOMR                       
115400*        END-IF                                                           
115500*        MOVE CLAG-ADLAGOMR-SVS TO WY-ADLAGOMR                            
115600*        PERFORM S02-ADLAGOMR-KOLL                                        
115700*        MOVE RETT                  TO FALT-ADPLATS-SVS                   
115800*        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-SVS-OLD-ATTR           
115900*        MOVE MID-ADPLATS-SVS       TO MOD-ADPLATS-SVS-OLD                
116000*        MOVE JA                    TO ARTC-UPPD-SW                       
116100*      ELSE                                                               
116200*        MOVE FEL                   TO FALT-ADPLATS-SVS                   
116300*        MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
116400*      END-IF                                                             
116500*    END-IF                                                               
116600                                                                          
116700     IF MID-ADGANG-CD(1) = ALL '+'                                        
116800       MOVE INGET-IFYLLT            TO FALT-ADGANG-CD1                    
116900     ELSE                                                                 
117000       IF MID-ADGANG-CD(1) NUMERIC AND                                    
117100                                    CLAG-ADLAGOMR-CD(1) > 0               
117200         MOVE CLAG-ADLAGOMR-CD(1) TO WX-ADLAGOMR                          
117300                                          WY-ADLAGOMR                     
117400         PERFORM S02-ADLAGOMR-KOLL                                        
117500          MOVE RETT                  TO FALT-ADGANG-CD1                   
117600          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-CD-OLD-ATTR(1)         
117700          MOVE MID-ADGANG-CD(1)      TO MOD-ADGANG-CD-OLD(1)              
117800          MOVE JA                    TO ARTC-UPPD-SW                      
117900       ELSE                                                               
118000         MOVE FEL                   TO FALT-ADGANG-CD1                    
118100         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
118200       END-IF                                                             
118300     END-IF                                                               
118400                                                                          
118500     IF MID-ADPLATS-CD(1) = ALL '+'                                       
118600       MOVE INGET-IFYLLT            TO FALT-ADPLATS-CD1                   
118700     ELSE                                                                 
118800       INSPECT MID-ADPLATS-CD(1) REPLACING LEADING SPACE BY ZERO          
118900       IF MID-ADPLATS-CD(1) NUMERIC   AND                                 
119000                                CLAG-ADLAGOMR-CD(1) > 0                   
119100         MOVE CLAG-ADLAGOMR-CD(1) TO WX-ADLAGOMR                          
119200                                          WY-ADLAGOMR                     
119300         PERFORM S02-ADLAGOMR-KOLL                                        
119400          MOVE RETT                  TO FALT-ADPLATS-CD1                  
119500          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-CD-OLD-ATTR(1)        
119600          MOVE MID-ADPLATS-CD(1)     TO MOD-ADPLATS-CD-OLD(1)             
119700          MOVE JA                    TO ARTC-UPPD-SW                      
119800       ELSE                                                               
119900         MOVE FEL                   TO FALT-ADPLATS-CD1                   
120000         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
120100       END-IF                                                             
120200     END-IF                                                               
120300                                                                          
120400                                                                          
120500     IF MID-ADGANG-CD(2) = ALL '+'                                        
120600       MOVE INGET-IFYLLT            TO FALT-ADGANG-CD2                    
120700     ELSE                                                                 
120800       IF MID-ADGANG-CD(2) NUMERIC AND                                    
120900                                    CLAG-ADLAGOMR-CD(2) > 0               
121000         MOVE CLAG-ADLAGOMR-CD(2) TO WX-ADLAGOMR                          
121100                                          WY-ADLAGOMR                     
121200         PERFORM S02-ADLAGOMR-KOLL                                        
121300         MOVE RETT                  TO FALT-ADGANG-CD2                    
121400         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-CD-OLD-ATTR(2)          
121500         MOVE MID-ADGANG-CD(2)      TO MOD-ADGANG-CD-OLD(2)               
121600         MOVE JA                    TO ARTC-UPPD-SW                       
121700       ELSE                                                               
121800         MOVE FEL                   TO FALT-ADGANG-CD2                    
121900         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
122000       END-IF                                                             
122100     END-IF                                                               
122200                                                                          
122300     IF MID-ADPLATS-CD(2) = ALL '+'                                       
122400       MOVE INGET-IFYLLT            TO FALT-ADPLATS-CD2                   
122500     ELSE                                                                 
122600       INSPECT MID-ADPLATS-CD(2) REPLACING LEADING SPACE BY ZERO          
122700       IF MID-ADPLATS-CD(2) NUMERIC  AND                                  
122800                                  CLAG-ADLAGOMR-CD(2) > 0                 
122900         MOVE CLAG-ADLAGOMR-CD(2) TO WX-ADLAGOMR                          
123000                                          WY-ADLAGOMR                     
123100         PERFORM S02-ADLAGOMR-KOLL                                        
123200         MOVE RETT                  TO FALT-ADPLATS-CD2                   
123300         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-CD-OLD-ATTR(2)         
123400         MOVE MID-ADPLATS-CD(2)     TO MOD-ADPLATS-CD-OLD(2)              
123500         MOVE JA                    TO ARTC-UPPD-SW                       
123600       ELSE                                                               
123700         MOVE FEL                   TO FALT-ADPLATS-CD2                   
123800         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
123900       END-IF                                                             
124000     END-IF                                                               
124100                                                                          
124200                                                                          
124300     IF MID-ADGANG-CD(3) = ALL '+'                                        
124400       MOVE INGET-IFYLLT            TO FALT-ADGANG-CD3                    
124500     ELSE                                                                 
124600       IF MID-ADGANG-CD(3) NUMERIC AND                                    
124700                                  CLAG-ADLAGOMR-CD(3) > 0                 
124800         MOVE CLAG-ADLAGOMR-CD(3) TO WX-ADLAGOMR                          
124900                                          WY-ADLAGOMR                     
125000         PERFORM S02-ADLAGOMR-KOLL                                        
125100         MOVE RETT                  TO FALT-ADGANG-CD3                    
125200         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-CD-OLD-ATTR(3)          
125300         MOVE MID-ADGANG-CD(3)      TO MOD-ADGANG-CD-OLD(3)               
125400         MOVE JA                    TO ARTC-UPPD-SW                       
125500       ELSE                                                               
125600         MOVE FEL                   TO FALT-ADGANG-CD3                    
125700         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
125800       END-IF                                                             
125900     END-IF                                                               
126000                                                                          
126100     IF MID-ADPLATS-CD(3) = ALL '+'                                       
126200       MOVE INGET-IFYLLT            TO FALT-ADPLATS-CD3                   
126300     ELSE                                                                 
126400       INSPECT MID-ADPLATS-CD(3) REPLACING LEADING SPACE BY ZERO          
126500       IF MID-ADPLATS-CD(3) NUMERIC AND                                   
126600                                   CLAG-ADLAGOMR-CD(3) > 0                
126700         MOVE CLAG-ADLAGOMR-CD(3) TO WX-ADLAGOMR                          
126800                                          WY-ADLAGOMR                     
126900         PERFORM S02-ADLAGOMR-KOLL                                        
127000         MOVE RETT                  TO FALT-ADPLATS-CD3                   
127100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-CD-OLD-ATTR(3)         
127200         MOVE MID-ADPLATS-CD(3)     TO MOD-ADPLATS-CD-OLD(3)              
127300         MOVE JA                    TO ARTC-UPPD-SW                       
127400       ELSE                                                               
127500         MOVE FEL                   TO FALT-ADPLATS-CD3                   
127600         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
127700       END-IF                                                             
127800     END-IF                                                               
127900                                                                          
128000                                                                          
128100     IF MID-ADGANG-CD(4) = ALL '+'                                        
128200       MOVE INGET-IFYLLT            TO FALT-ADGANG-CD4                    
128300     ELSE                                                                 
128400       IF MID-ADGANG-CD(4) NUMERIC AND                                    
128500                                   CLAG-ADLAGOMR-CD(4) > 0                
128600         MOVE CLAG-ADLAGOMR-CD(4) TO WX-ADLAGOMR                          
128700                                          WY-ADLAGOMR                     
128800         PERFORM S02-ADLAGOMR-KOLL                                        
128900         MOVE RETT                  TO FALT-ADGANG-CD4                    
129000         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGANG-CD-OLD-ATTR(4)          
129100         MOVE MID-ADGANG-CD(4)      TO MOD-ADGANG-CD-OLD(4)               
129200         MOVE JA                    TO ARTC-UPPD-SW                       
129300       ELSE                                                               
129400         MOVE FEL                   TO FALT-ADGANG-CD4                    
129500         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
129600       END-IF                                                             
129700     END-IF                                                               
129800                                                                          
129900     IF MID-ADPLATS-CD(4) = ALL '+'                                       
130000       MOVE INGET-IFYLLT            TO FALT-ADPLATS-CD4                   
130100     ELSE                                                                 
130200       INSPECT MID-ADPLATS-CD(4) REPLACING LEADING SPACE BY ZERO          
130300       IF MID-ADPLATS-CD(4) NUMERIC  AND                                  
130400                                    CLAG-ADLAGOMR-CD(4) > 0               
130500         MOVE CLAG-ADLAGOMR-CD(4) TO WX-ADLAGOMR                          
130600                                          WY-ADLAGOMR                     
130700         PERFORM S02-ADLAGOMR-KOLL                                        
130800         MOVE RETT                  TO FALT-ADPLATS-CD4                   
130900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPLATS-CD-OLD-ATTR(4)         
131000         MOVE MID-ADPLATS-CD(4)     TO MOD-ADPLATS-CD-OLD(4)              
131100         MOVE JA                    TO ARTC-UPPD-SW                       
131200       ELSE                                                               
131300         MOVE FEL                   TO FALT-ADPLATS-CD4                   
131400         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
131500       END-IF                                                             
131600     END-IF                                                               
131700                                                                          
134900                                                                          
135000     IF MID-IDARTNR-EMBQ3 = ALL '+'                                       
135100       MOVE INGET-IFYLLT             TO FALT-IDARTNR-EMBQ3                
135200     ELSE                                                                 
135300       INSPECT MID-IDARTNR-EMBQ3 REPLACING LEADING SPACE BY ZERO          
135400       IF  MID-IDARTNR-EMBQ3 NUMERIC                                      
135500         MOVE RETT                  TO FALT-IDARTNR-EMBQ3                 
135600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-EMBQ3-OLD-ATTR         
135700         MOVE MID-IDARTNR-EMBQ3     TO MOD-IDARTNR-EMBQ3-OLD              
135800         MOVE JA                    TO ARTC-UPPD-SW                       
135900       ELSE                                                               
136000         MOVE FEL                   TO FALT-IDARTNR-EMBQ3                 
136100         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
136200       END-IF                                                             
136300     END-IF                                                               
136400                                                                          
136500     IF MID-IDARTNR-EMBQ4 = ALL '+'                                       
136600       MOVE INGET-IFYLLT            TO FALT-IDARTNR-EMBQ4                 
136700     ELSE                                                                 
136800       INSPECT MID-IDARTNR-EMBQ4 REPLACING LEADING SPACE BY ZERO          
136900       IF  MID-IDARTNR-EMBQ4 NUMERIC                                      
137000         MOVE RETT                  TO FALT-IDARTNR-EMBQ4                 
137100         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-EMBQ4-OLD-ATTR         
137200         MOVE MID-IDARTNR-EMBQ4     TO MOD-IDARTNR-EMBQ4-OLD              
137300         MOVE JA                    TO ARTC-UPPD-SW                       
137400       ELSE                                                               
137500         MOVE FEL                   TO FALT-IDARTNR-EMBQ4                 
137600         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
137700       END-IF                                                             
137800     END-IF                                                               
137900                                                                          
141200     IF MID-KDSPEEMB = ALL '+'                                            
141300       MOVE INGET-IFYLLT             TO FALT-KDSPEEMB                     
141400     ELSE                                                                 
141500       IF MID-KDSPEEMB NUMERIC                                            
141600         MOVE RETT                  TO FALT-KDSPEEMB                      
141700         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSPEEMB-OLD-ATTR              
141800         MOVE MID-KDSPEEMB          TO MOD-KDSPEEMB-OLD                   
141900         MOVE JA                    TO ARTC-UPPD-SW                       
142000       ELSE                                                               
142100         MOVE FEL                   TO FALT-KDSPEEMB                      
142200         MOVE FEL-5 (TYP)           TO MOD-TEMFSFEL                       
142300       END-IF                                                             
142400     END-IF                                                               
142500                                                                          
142600     IF MID-FLEJBUFF = ALL '+'                                            
142700       MOVE INGET-IFYLLT            TO FALT-FLEJBUFF                      
142800     ELSE                                                                 
142900       IF MID-FLEJBUFF = JA OR YES OR NEJ                                 
143000          MOVE RETT                  TO FALT-FLEJBUFF                     
143100          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEJBUFF-OLD-ATTR             
143200          MOVE MID-FLEJBUFF          TO MOD-FLEJBUFF-OLD                  
143300          MOVE JA                    TO ARTC-UPPD-SW                      
143400       ELSE                                                               
143500         MOVE FEL                    TO FALT-FLEJBUFF                     
143600         MOVE FEL-5 (TYP)            TO MOD-TEMFSFEL                      
143700       END-IF                                                             
143800     END-IF                                                               
143900                                                                          
144000*    IF MID-ADINLOMR-BOA NOT = ALL '+'                                    
144100     IF MID-ADINLOMR-BOA = ALL '+'                                        
144200       MOVE INGET-IFYLLT             TO FALT-ADINLOMR-BOA                 
144300     ELSE                                                                 
144400        IF MID-ADINLOMR-BOA = ALL SPACES                                  
144500           MOVE RETT                 TO FALT-ADINLOMR-BOA                 
144600           MOVE JA                   TO ARTC-UPPD-SW                      
144700           MOVE MFS-ADD-LYS-UPP-FAELT                                     
144800                                     TO MOD-ADINLOMR-BOA-OLD-ATTR         
144900           MOVE SPACES               TO MOD-ADINLOMR-BOA-OLD              
145000        ELSE                                                              
145100           MOVE MID-ADINLOMR-BOA     TO W-ADINLOMR                        
145200           PERFORM IMS-GU-PLAA-6006                                       
145300           IF SEGMENT-FINNS                                               
145400              IF PLAA-6006-KDINLOMR = 'BO'                                
145500                 MOVE RETT           TO FALT-ADINLOMR-BOA                 
145600                 MOVE MFS-ADD-LYS-UPP-FAELT                               
145700                                     TO MOD-ADINLOMR-BOA-OLD-ATTR         
145800                 MOVE MID-ADINLOMR-BOA                                    
145900                                     TO MOD-ADINLOMR-BOA-OLD              
146000                 MOVE JA             TO ARTC-UPPD-SW                      
146100              ELSE                                                        
146200                 MOVE FEL            TO FALT-ADINLOMR-BOA                 
146300                 MOVE FEL-13(TYP)    TO MOD-TEMFSFEL                      
146400              END-IF                                                      
146500           ELSE                                                           
146600              MOVE FEL               TO FALT-ADINLOMR-BOA                 
146700              MOVE FEL-13(TYP)       TO MOD-TEMFSFEL                      
146800           END-IF                                                         
146900        END-IF                                                            
147000     END-IF                                                               
147100                                                                          
147200     MOVE  +1  TO IX                                                      
147300     PERFORM UNTIL IX > RAD-MAX OR FALT(IX) = 'F'                         
147400*      MOVE MFS-FORMATETS-ATTR   TO MOD-NEW-ATTR(1)                       
147500       MOVE MFS-RENSA-FAELT      TO MOD-NEW (IX)                          
147600       ADD +1 TO IX                                                       
147700     END-PERFORM                                                          
147800                                                                          
147900     IF ADLAGOMR-FEL                                                      
148000       MOVE  NEJ                   TO ARTC-UPPD-SW                        
148100       MOVE  FEL                   TO FALT-ADLAGOMR-CDC                   
148200       MOVE FEL-11(TYP)            TO MOD-TEMFSFEL                        
148300     END-IF                                                               
148400     .                                                                    
148500     EJECT                                                                
148600                                                                          
148700 E-UPPDATERA-BASER           SECTION.                                     
148800                                                                          
148900     IF SEGMENT-FINNS                                                     
149000       IF ARTC-UPPD                                                       
151000***  HÄR ÄR ÖVRIGA DATA TILL ARTC14                                       
151100         IF FALT-IDARTNR-EMBQ3 = RETT                                     
151200           MOVE MID-IDARTNR-EMBQ3 TO CLAG-IDARTNR-EMBQ3                   
151300                                     MOD619B-MID-IDARTNR-EMBQ3            
151400           MOVE JA                TO DISPATCH-SW                          
151500         END-IF                                                           
151600                                                                          
151700         IF FALT-IDARTNR-EMBQ4 = RETT                                     
151800           MOVE MID-IDARTNR-EMBQ4 TO CLAG-IDARTNR-EMBQ4                   
151900         END-IF                                                           
152000                                                                          
152100         IF FALT-FLEJBUFF = RETT                                          
152200           IF MID-FLEJBUFF = YES                                          
152300             MOVE JA               TO CLAG-FLEJBUFF                       
152400           ELSE                                                           
152500             MOVE MID-FLEJBUFF     TO CLAG-FLEJBUFF                       
152600           END-IF                                                         
152700         END-IF                                                           
152800                                                                          
152900         IF FALT-ADINLOMR-BOA = RETT                                      
153000            MOVE MID-ADINLOMR-BOA  TO CLAG-ADINLOMR-BOA                   
153100         END-IF                                                           
153200                                                                          
153300         PERFORM IMS-REPL-ARTC                                            
153400       END-IF                                                             
153500                                                                          
153600       IF ARTC-UPPD                                                       
153700         IF FALT-KDSPEEMB = RETT                                          
153800           MOVE MID-KDSPEEMB   TO CLAG-KDSPEEMB                           
153900         END-IF                                                           
154000                                                                          
154100         IF FALT-ADLAGOMR-CDC = RETT                                      
154200           MOVE MID-ADLAGOMR-CDC TO CLAG-ADLAGOMR                         
154300                                  MOD619B-MID-ADLAGOMR                    
154400           MOVE JA             TO DISPATCH-SW                             
154500         END-IF                                                           
154600         MOVE CLAG-ADLAGOMR       TO WS-ADLAGOMR                          
154700                                                                          
154800         IF FALT-ADGANG-CDC = RETT                                        
154900           MOVE MID-ADGANG-CDC TO CLAG-ADGANG                             
155000                                  MOD619B-MID-ADGANG                      
155100           MOVE JA             TO DISPATCH-SW                             
155200         END-IF                                                           
155300         MOVE CLAG-ADGANG         TO WS-ADGANG                            
155400                                                                          
155500         IF FALT-ADPLATS-CDC = RETT                                       
155600           MOVE MID-ADPLATS-CDC TO CLAG-ADPLATS                           
155700                                  MOD619B-MID-ADPLATS                     
155800           MOVE JA             TO DISPATCH-SW                             
155900         END-IF                                                           
156000         MOVE CLAG-ADPLATS        TO WS-ADPLATS                           
156100                                                                          
156200         IF FALT-KVMAXPL = RETT                                           
156300           MOVE MID-KVMAXPL     TO CLAG-KVMAXPL                           
156400           IF CLAG-ADLAGOMR = 20 OR                                       
156401             (CLAG-ADLAGOMR = 31 AND CLAG-ADGANG > 39 AND                 
156402                                 CLAG-ADGANG < 42) OR                     
156403             (CLAG-ADLAGOMR                                               
156404             = 11 AND ((CLAG-ADGANG = 2 AND (CLAG-ADPLATS > 104           
156405                                   AND CLAG-ADPLATS < 136)                
156406                                   AND (WS-ADPLATS (5:1) =                
156407                                   1 OR 3 OR 5 OR 7 OR 9)) OR             
156408                      (CLAG-ADGANG = 7 AND (CLAG-ADPLATS > 390            
156409                                   AND CLAG-ADPLATS < 470)                
156410                                   AND (WS-ADPLATS (5:1) =                
156411                                   1 OR 3 OR 5 OR 7 OR 9))))              
156412             IF CLAG-KVQPACK-3 > 1                                        
156413               COMPUTE CLAG-KVREFPKT-PLOCK =                              
156414               CLAG-KVMAXPL -(CLAG-KVQPACK-3 * 2)                         
156415               COMPUTE CLAG-KVREFBER-PLOCK =                              
156416               (CLAG-KVQPACK-3 * 2)                                       
156417             END-IF                                                       
156418           ELSE                                                           
156419                IF CLAG-ADLAGOMR = 21 OR                                  
156420                   (CLAG-ADLAGOMR = 22 AND                                
156430                   CLAG-ADGANG > 8 AND                                    
156501                   CLAG-ADGANG < 39) OR                                   
156502                  (CLAG-ADLAGOMR = 11 AND                                 
156503                   CLAG-ADGANG > 1 AND                                    
156504                   CLAG-ADGANG < 8) OR                                    
156509                  (CLAG-ADLAGOMR = 10 AND                                 
156510                   CLAG-ADGANG > 9 AND                                    
156520                   CLAG-ADGANG < 36)                                      
156716                  IF CLAG-KVQPACK-3 > 1 AND                               
156720                     CLAG-ADLAGOMR NOT = 10                               
156801                   COMPUTE CLAG-KVREFPKT-PLOCK =                          
156802                           CLAG-KVMAXPL - CLAG-KVQPACK-3                  
156803                           MOVE CLAG-KVQPACK-3 TO                         
156804                            CLAG-KVREFBER-PLOCK                           
157001                  ELSE                                                    
157101                   COMPUTE CLAG-KVREFPKT-PLOCK =                          
157201                           CLAG-KVMAXPL * 0.2                             
157202                   COMPUTE CLAG-KVREFBER-PLOCK =                          
157203                           CLAG-KVMAXPL * 0.8                             
157204                  END-IF                                                  
157301                END-IF                                                    
157401           END-IF                                                         
157402           IF CLAG-KVREFPKT-PLOCK < 0                                     
157403             MOVE ZERO TO CLAG-KVREFPKT-PLOCK                             
157404           END-IF                                                         
157501         END-IF                                                           
157601                                                                          
157701*        IF FALT-ADLAGOMR-SVS = RETT                                      
157801*          MOVE MID-ADLAGOMR-SVS TO CLAG-ADLAGOMR-SVS                     
157901*        END-IF                                                           
158001*                                                                         
158101*        IF FALT-ADGANG-SVS = RETT                                        
158201*          MOVE MID-ADGANG-SVS TO CLAG-ADGANG-SVS                         
159000*        END-IF                                                           
160000*                                                                         
160100*        IF FALT-ADPLATS-SVS = RETT                                       
160200*          MOVE MID-ADPLATS-SVS TO CLAG-ADPLATS-SVS                       
160300*        END-IF                                                           
160400                                                                          
160500         IF FALT-ADGANG-CD1 = RETT                                        
160600           MOVE MID-ADGANG-CD(1) TO CLAG-ADGANG-CD(1)                     
160700                                  MOD619B-MID-ADGANG-CD(1)                
160800           MOVE JA             TO DISPATCH-SW                             
160900         END-IF                                                           
161000                                                                          
161100         IF FALT-ADPLATS-CD1 = RETT                                       
161200           MOVE MID-ADPLATS-CD(1) TO CLAG-ADPLATS-CD(1)                   
161300                                  MOD619B-MID-ADPLATS-CD(1)               
161400           MOVE JA             TO DISPATCH-SW                             
161500         END-IF                                                           
161600                                                                          
161700         IF FALT-ADGANG-CD2 = RETT                                        
161800           MOVE MID-ADGANG-CD(2) TO CLAG-ADGANG-CD(2)                     
161900                                  MOD619B-MID-ADGANG-CD(2)                
162000           MOVE JA             TO DISPATCH-SW                             
162100         END-IF                                                           
162200                                                                          
162300         IF FALT-ADPLATS-CD2 = RETT                                       
162400           MOVE MID-ADPLATS-CD(2) TO CLAG-ADPLATS-CD(2)                   
162500                                  MOD619B-MID-ADPLATS-CD(2)               
162600           MOVE JA             TO DISPATCH-SW                             
162700         END-IF                                                           
162800                                                                          
162900         IF FALT-ADGANG-CD3 = RETT                                        
163000           MOVE MID-ADGANG-CD(3) TO CLAG-ADGANG-CD(3)                     
163100                                  MOD619B-MID-ADGANG-CD(3)                
163200           MOVE JA             TO DISPATCH-SW                             
163300         END-IF                                                           
163400                                                                          
163500         IF FALT-ADPLATS-CD3 = RETT                                       
163600           MOVE MID-ADPLATS-CD(3) TO CLAG-ADPLATS-CD(3)                   
163700                                  MOD619B-MID-ADPLATS-CD(3)               
163800           MOVE JA             TO DISPATCH-SW                             
163900         END-IF                                                           
164000                                                                          
164100         IF FALT-ADGANG-CD4 = RETT                                        
164200           MOVE MID-ADGANG-CD(4) TO CLAG-ADGANG-CD(4)                     
164300                                  MOD619B-MID-ADGANG-CD(4)                
164400           MOVE JA             TO DISPATCH-SW                             
164500         END-IF                                                           
164600                                                                          
164700         IF FALT-ADPLATS-CD4 = RETT                                       
164800           MOVE MID-ADPLATS-CD(4) TO CLAG-ADPLATS-CD(4)                   
164900                                  MOD619B-MID-ADPLATS-CD(4)               
165000           MOVE JA             TO DISPATCH-SW                             
165100         END-IF                                                           
165200                                                                          
165300         PERFORM IMS-REPL-ARTC                                            
165400                                                                          
165500         MOVE CLAG-KVREFPKT-PLOCK TO MOD-KVREFPKT-PLOCK                   
252700                                                                          
252800         IF  FALT-ADLAGOMR-CDC = INGET-IFYLLT                             
252900         AND FALT-ADGANG-CDC = INGET-IFYLLT                               
253000         AND FALT-ADPLATS-CDC = INGET-IFYLLT                              
253100           CONTINUE                                                       
253200         ELSE                                                             
253300           IF FALT-ADLAGOMR-CDC = RETT                                    
253400           OR FALT-ADGANG-CDC = RETT                                      
253500           OR FALT-ADPLATS-CDC = RETT                                     
253600             PERFORM EB-STARTA-W40289                                     
253700           END-IF                                                         
253800         END-IF                                                           
253900                                                                          
254000***      UPPDATERING AV PLATSREGISTRET (WDJ9) FÖR CDC                     
254100         IF FALT-ADLAGOMR-CDC = INGET-IFYLLT    AND                       
254200            FALT-ADGANG-CDC = INGET-IFYLLT      AND                       
254300            FALT-ADPLATS-CDC = INGET-IFYLLT                               
254400           CONTINUE                                                       
254500         ELSE                                                             
254600           PERFORM IMS-GU-LOCB01                                          
254700           IF SEGMENT-SAKNAS                                              
254800             MOVE W-IDARTNR TO LOCB-ART-IDARTNR                           
254900             PERFORM IMS-ISRT-LOCB01                                      
255000             PERFORM IMS-GU-LOCB01                                        
255100           END-IF                                                         
255200           IF SEGMENT-FINNS                                               
255300             PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'C'        
255400             PERFORM IMS-GHNP-LOCB11                                      
255500               IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'C'                 
255600                 MOVE FUNCTION CURRENT-DATE(1:8) TO                       
255700                                               LOCB-HIST-DASTODAT         
255800                 MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO                 
255900                 PERFORM IMS-REPL-LOCB11                                  
256000               END-IF                                                     
256100             END-PERFORM                                                  
256200             MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM               
256300             MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                 
256400             COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -               
256500                                                        LOGG-DATUM        
256600             COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID        
256700             MOVE WC-CDC-SE          TO LOCB-HIST-IDDC                    
256800             IF FALT-ADLAGOMR-CDC = RETT                                  
256900               MOVE MID-ADLAGOMR-CDC TO LOCB-HIST-ADLAGOMR                
257000             ELSE                                                         
257100               MOVE W-ADLAGOMR-WDJ9-CDC                                   
257200                                     TO LOCB-HIST-ADLAGOMR                
257300             END-IF                                                       
257400             IF FALT-ADGANG-CDC = RETT                                    
257500               MOVE MID-ADGANG-CDC   TO LOCB-HIST-ADGANG                  
257600             ELSE                                                         
257700               MOVE W-ADGANG-WDJ9-CDC                                     
257800                                     TO LOCB-HIST-ADGANG                  
257900             END-IF                                                       
258000             IF FALT-ADPLATS-CDC = RETT                                   
258100               MOVE MID-ADPLATS-CDC  TO LOCB-HIST-ADPLATS                 
258200             ELSE                                                         
258300               MOVE W-ADPLATS-WDJ9-CDC                                    
258400                                     TO LOCB-HIST-ADPLATS                 
258500             END-IF                                                       
258600             MOVE PRIME-LOCATION-CDC TO LOCB-HIST-KDLOC                   
258700             MOVE MSGI-IDUSER        TO LOCB-HIST-IDUSER                  
258800             MOVE SPACE              TO LOCB-HIST-IDUSER-STO              
258900             MOVE ZERO               TO LOCB-HIST-DASTODAT                
259000             PERFORM IMS-ISRT-LOCB11                                      
259100           END-IF                                                         
259200         END-IF                                                           
259300                                                                          
259400***      UPPDATERING AV PLATSREGISTRET (WDJ9) FÖR SVS                     
259500*        IF FALT-ADLAGOMR-SVS = INGET-IFYLLT    AND                       
259600*           FALT-ADGANG-SVS = INGET-IFYLLT      AND                       
259700*           FALT-ADPLATS-SVS = INGET-IFYLLT                               
259800*          CONTINUE                                                       
259900*        ELSE                                                             
260000*          PERFORM IMS-GU-LOCB01                                          
260100*          IF SEGMENT-SAKNAS                                              
260200*            MOVE W-IDARTNR TO LOCB-ART-IDARTNR                           
260300*            PERFORM IMS-ISRT-LOCB01                                      
260400*            PERFORM IMS-GU-LOCB01                                        
260500*          END-IF                                                         
260600*          IF SEGMENT-FINNS                                               
260700*            PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'S'        
260800*            PERFORM IMS-GHNP-LOCB11                                      
260900*              IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'S'                 
261000*                MOVE FUNCTION CURRENT-DATE(1:8) TO                       
261100*                                              LOCB-HIST-DASTODAT         
261200*                MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO                 
261300*                PERFORM IMS-REPL-LOCB11                                  
261400*              END-IF                                                     
261500*            END-PERFORM                                                  
261600*            MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM               
261700*            MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                 
261800*            COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -               
261900*                                                       LOGG-DATUM        
262000*            COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID        
262100*            MOVE WC-CDC-SE          TO LOCB-HIST-IDDC                    
262200*            IF FALT-ADLAGOMR-SVS = RETT                                  
262300*              MOVE MID-ADLAGOMR-SVS TO LOCB-HIST-ADLAGOMR                
262400*            ELSE                                                         
262500*              MOVE W-ADLAGOMR-WDJ9-SVS                                   
262600*                                    TO LOCB-HIST-ADLAGOMR                
262700*            END-IF                                                       
262800*            IF FALT-ADGANG-SVS = RETT                                    
262900*              MOVE MID-ADGANG-SVS   TO LOCB-HIST-ADGANG                  
263000*            ELSE                                                         
263100*              MOVE W-ADGANG-WDJ9-SVS                                     
263200*                                    TO LOCB-HIST-ADGANG                  
263300*            END-IF                                                       
263400*            IF FALT-ADPLATS-SVS = RETT                                   
263500*              MOVE MID-ADPLATS-SVS  TO LOCB-HIST-ADPLATS                 
263600*            ELSE                                                         
263700*              MOVE W-ADPLATS-WDJ9-SVS                                    
263800*                                    TO LOCB-HIST-ADPLATS                 
263900*            END-IF                                                       
264000*            MOVE PRIME-LOCATION-SVS TO LOCB-HIST-KDLOC                   
264100*            MOVE MSGI-IDUSER        TO LOCB-HIST-IDUSER                  
264200*            MOVE SPACE              TO LOCB-HIST-IDUSER-STO              
264300*            MOVE ZERO               TO LOCB-HIST-DASTODAT                
264400*            PERFORM IMS-ISRT-LOCB11                                      
264500*          END-IF                                                         
264600*        END-IF                                                           
264700       END-IF                                                             
264800                                                                          
264900                                                                          
265000       MOVE RETT-1 (TYP) TO MOD-TEMFSINF                                  
265100     ELSE                                                                 
265200       MOVE +1 TO IX                                                      
265300       PERFORM UNTIL IX > RAD-MAX                                         
265400         MOVE MFS-STAENG-FAELT TO MOD-NEW-ATTR (IX)                       
265500         ADD +1 TO IX                                                     
265600       END-PERFORM                                                        
265700       MOVE FEL-2 (TYP) TO MOD-TEMFSFEL                                   
265800     END-IF                                                               
265900                                                                          
266000     IF STARTA-DISPATCH                                                   
266100       PERFORM EA-STARTA-DISPATCHEN                                       
266200     END-IF                                                               
266300     .                                                                    
266400     EJECT                                                                
266500                                                                          
266600 EA-STARTA-DISPATCHEN SECTION.                                            
266700                                                                          
266800     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
266900     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
267000     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
267100     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
267200     MOVE SPACE                TO MSG-KOM-KDTRANS                         
267300     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
267400     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
267500     MOVE 'W6016300'           TO MSG-KOM-IDSNDJOB                        
267600     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
267700     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
267800     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
267900                                                                          
268000     MOVE IDARTNR-WS           TO MOD619B-MID-IDARTNR                     
268100     MOVE W-IDDC               TO MOD619B-MID-IDDC                        
268200     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
268300     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
268400     MOVE '6163'               TO P-TO-P-IDTRANS                          
268500     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
268600     MOVE MOD619B-MID-W6I19B01 TO P-TO-P-DATA                             
268700                                                                          
268800     CALL W006KOM USING MSG-PCB                                           
268900                        DISP-PCB                                          
269000                        KOM-KOMA-PCB                                      
269100                        MSG-KOM-WMSGKOM                                   
269200                        P-TO-P-SW                                         
269300     .                                                                    
269400     SKIP2                                                                
269500 EB-STARTA-W40289 SECTION.                                                
269600                                                                          
269700     IF WS-ADLAGOMR > ZERO                                                
269800     OR WS-ADGANG   > ZERO                                                
269900     OR WS-ADPLATS  > ZERO                                                
270000                                                                          
270100       MOVE IDARTNR-WS         TO P-TO-P2-MID-IDARTNR-IN                  
270200       MOVE WC-CDC-SE          TO P-TO-P2-MID-IDDC-IN                     
270300       MOVE WS-ADLAGOMR        TO P-TO-P2-MID-ADLAGOMR-IN                 
270400       MOVE WS-ADGANG          TO P-TO-P2-MID-ADGANG-IN                   
270500       MOVE WS-ADPLATS         TO P-TO-P2-MID-ADPLATS-IN                  
270600       MOVE ZERO               TO P-TO-P2-MID-IDDISTR-IN                  
270700                                  P-TO-P2-MID-IDKUNDNR-IN                 
270800                                  P-TO-P2-MID-IDORDNR5-IN                 
270900                                  P-TO-P2-MID-IDORDER-IN                  
271000                                  P-TO-P2-MID-IDLOPNR-IN                  
271100                                                                          
271200       COMPUTE P-TO-P2-KVLL =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
271300       END-COMPUTE                                                        
271400       MOVE 'W4T289X '         TO P-TO-P2-KDTRANS                         
271500       MOVE '6163'             TO P-TO-P2-IDTRANS                         
271600       MOVE MFS-KDMFSFOR       TO P-TO-P2-KDMFSFOR                        
271700                                                                          
271800       PERFORM IMS-PURG-4289                                              
271900     END-IF                                                               
272000     .                                                                    
272100     EJECT                                                                
272200 EC-KONTROLL-BYTES SECTION.                                               
272300                                                                          
272400     IF BYT02-RENOV                                                       
272500       IF BYT16-BYTES                                                     
272600          COMPUTE TEST-IDARTNR = TEST-IDARTNR +                           
272700                                 6000                                     
272800          END-COMPUTE                                                     
272900       ELSE                                                               
273000          COMPUTE TEST-IDARTNR = TEST-IDARTNR +                           
273100                                 1000                                     
273200          END-COMPUTE                                                     
273300       END-IF                                                             
273400       MOVE JA TO CORE-SW                                                 
273500       MOVE TEST-IDARTNR      TO W-IDARTNR2                               
273600     END-IF                                                               
273700     .                                                                    
273800     EJECT                                                                
273900 F-KONTROLLERA-PLATS SECTION.                                             
274000                                                                          
274100     IF MID-ADLAGOMR-CDC = ALL '0' OR MID-ADPLATS-CDC = ALL '0'           
274200       IF MID-ADLAGOMR-CDC = '43' OR '48' OR '49' OR '58' OR              
274300                         '90' OR '91' OR '92'                             
274400         CONTINUE                                                         
274500       ELSE                                                               
274600         IF CLAG-KVLS            = 0                                      
274700           AND CLAG-KVEFRS       = 0                                      
274800           AND CLAG-KVAKS-CDC = 0                                         
274900           AND CLAG-KVAKS-PAV = 0                                         
275000           AND CLAG-KVAKS-T = 0                                           
275100           CONTINUE                                                       
275200         ELSE                                                             
275300           MOVE FEL       TO FALT-ADLAGOMR-CDC                            
275400                             FALT-ADPLATS-CDC                             
275500           MOVE FEL-8 (TYP) TO MOD-TEMFSFEL                               
275600         END-IF                                                           
275700       END-IF                                                             
275800     END-IF                                                               
275900*                                                                         
276000*    IF MID-ADLAGOMR-SVS = ALL '0' OR MID-ADPLATS-SVS = ALL '0'           
276100*      IF CLAG-KVLS-SVS        = 0                                        
276200*        CONTINUE                                                         
276300*      ELSE                                                               
276400*        MOVE FEL       TO FALT-ADLAGOMR-SVS                              
276500*                          FALT-ADPLATS-SVS                               
276600*        MOVE FEL-8 (TYP) TO MOD-TEMFSFEL                                 
276700*      END-IF                                                             
276800*    END-IF                                                               
276900*                                                                         
278600                                                                          
278700     .                                                                    
278800     EJECT                                                                
278900                                                                          
279000 G-SKRIV-PLATSSAETTARLISTA  SECTION.                                      
279100                                                                          
279200     IF TOM-MID                                                           
279300       PERFORM GA-KOLLA-PRINTER                                           
279400       IF PRINTER-OK AND ARTIKEL-OK                                       
279500         PERFORM GB-STARTA-W6019A                                         
279600         MOVE RETT-4(TYP)          TO MOD-TEMFSFEL                        
279700       ELSE                                                               
279800         IF ARTIKEL-OK                                                    
279900           MOVE FEL-9(TYP)         TO MOD-TEMFSFEL                        
280000           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR               
280100           MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-PRT                    
280200         END-IF                                                           
280300       END-IF                                                             
280400     ELSE                                                                 
280500       MOVE RETT-2(TYP)            TO MOD-TEMFSFEL                        
280600       PERFORM S01-NAGON-FEL                                              
280700     END-IF                                                               
280800     .                                                                    
280900     EJECT                                                                
281000                                                                          
281100 GA-KOLLA-PRINTER SECTION.                                                
281200                                                                          
281300     MOVE JA                   TO PRINTER-SW                              
281400     MOVE '001'                TO PRT-KDCALL                              
281500     MOVE SPACE                TO PRT-IDPRTLST                            
281600     MOVE SPACE                TO PRT-IDLTERM                             
281700     MOVE '6M'                 TO PRT-IDPRTLST(1:2)                       
281800     MOVE MID-ADINLOMR-PRT     TO PRT-IDPRTLST(3:6)                       
281900     CALL W006PRT  USING PRT-W006PRT                                      
282000     IF PRT-KDSVAR = 'R'                                                  
282100       MOVE PRT-BEPRTLST     TO MOD-TEMFSFEL                              
282200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-PRT-ATTR                 
282300     ELSE                                                                 
282400       MOVE NEJ              TO PRINTER-SW                                
282500     END-IF                                                               
282600     .                                                                    
282700     EJECT                                                                
282800                                                                          
282900 GB-STARTA-W6019A SECTION.                                                
283000                                                                          
283100     MOVE PRT-IDPRTLST         TO MOD619A-MID-IDPRTLST                    
283200     MOVE 'W6016300'           TO MOD619A-MID-IDPGM                       
283300     MOVE 1                    TO MOD619A-MID-KVPOST                      
283400     MOVE IDARTNR-WS           TO MOD619A-MID-IDARTNR      (1)            
283500     MOVE ZERO                 TO MOD619A-MID-IDOKOLLI     (1)            
283600                                  MOD619A-MID-IDINLVGN     (1)            
283700                                  MOD619A-MID-IDILIST      (1)            
283800                                  MOD619A-MID-KVRADER      (1)            
283900     MOVE SPACE                TO MOD619A-MID-ADINLOMR     (1)            
284000                                  MOD619A-MID-IDLEVNR-KOLLI(1)            
284100                                                                          
284200     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 55                  
284300     MOVE 'W6T19AX '           TO P-TO-P-KDTRANS                          
284400     MOVE '6163'               TO P-TO-P-IDTRANS                          
284500     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
284600     MOVE MOD619A-MID-W6I19A01 TO P-TO-P-DATA                             
284700                                                                          
284800     PERFORM IMS-ISRT-ALT-MSG                                             
284900     .                                                                    
285000     EJECT                                                                
285100                                                                          
285200 S01-NAGON-FEL SECTION.                                                   
285300                                                                          
285400     MOVE MFS-FORMATETS-ATTR TO MOD-ADLAGOMR-CDC-OLD-ATTR                 
285500                                MOD-ADGANG-CDC-OLD-ATTR                   
285600                                MOD-ADPLATS-CDC-OLD-ATTR                  
285700                                MOD-KVMAXPL-OLD-ATTR                      
285800*                               MOD-ADLAGOMR-SVS-OLD-ATTR                 
285900*                               MOD-ADGANG-SVS-OLD-ATTR                   
286000*                               MOD-ADPLATS-SVS-OLD-ATTR                  
286100                                MOD-VKART-OLD-ATTR                        
286200                                MOD-VLARTNTO-OLD-ATTR                     
286300                                MOD-KDVSOP-OLD-ATTR                       
286400                                MOD-KDSPEEMB-OLD-ATTR                     
286500                                MOD-IDARTNR-EMBQ3-OLD-ATTR                
286600                                MOD-IDARTNR-EMBQ4-OLD-ATTR                
286700                                MOD-FLEJBUFF-OLD-ATTR                     
286800                                MOD-ADINLOMR-BOA-OLD-ATTR                 
286900                                MOVE 1 TO CD-IX                           
287000                                PERFORM UNTIL CD-IX > 4                   
287100     MOVE MFS-FORMATETS-ATTR TO   MOD-ADLAGOMR-CD-OLD-ATTR(CD-IX)         
287200                                  MOD-ADGANG-CD-OLD-ATTR(CD-IX)           
287300                                  MOD-ADPLATS-CD-OLD-ATTR(CD-IX)          
287400                                  ADD 1 TO CD-IX                          
287500                                END-PERFORM                               
287600     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADLAGOMR-CDC-OLD                      
287700                                MOD-ADGANG-CDC-OLD                        
287800                                MOD-ADPLATS-CDC-OLD                       
287900                                MOD-KVMAXPL-OLD                           
288000*                               MOD-ADLAGOMR-SVS-OLD                      
288100*                               MOD-ADGANG-SVS-OLD                        
288200*                               MOD-ADPLATS-SVS-OLD                       
288300                                MOD-VKART-OLD                             
288400                                MOD-VLARTNTO-OLD                          
288500                                MOD-KDVSOP-OLD                            
288600                                MOD-KDSPEEMB-OLD                          
288700                                MOD-IDARTNR-EMBQ3-OLD                     
288800                                MOD-IDARTNR-EMBQ4-OLD                     
288900                                MOD-FLEJBUFF-OLD                          
289000                                MOD-ADINLOMR-BOA-OLD                      
289100                                MOVE 1 TO CD-IX                           
289200                                PERFORM UNTIL CD-IX > 4                   
289300     MOVE MFS-ROER-EJ-FAELT  TO   MOD-ADLAGOMR-CD-OLD(CD-IX)              
289400                                  MOD-ADGANG-CD-OLD(CD-IX)                
289500                                  MOD-ADPLATS-CD-OLD(CD-IX)               
289600                                  ADD 1 TO CD-IX                          
289700                                END-PERFORM                               
289800                                                                          
289900     MOVE +1 TO IX                                                        
290000*    PERFORM  UNTIL IX > RAD-MAX                                          
290100     PERFORM  UNTIL IX > 23                                               
290200                                                                          
290300       IF FALT (IX) = 'R' OR 'I'                                          
290400         MOVE MFS-NUM-FAELT-RAETT TO MOD-NEW-ATTR (IX)                    
290500       ELSE                                                               
290600         MOVE MFS-NUM-FAELT-FEL TO MOD-NEW-ATTR (IX)                      
290900       END-IF                                                             
291000                                                                          
291100       MOVE MFS-ROER-EJ-FAELT TO MOD-NEW (IX)                             
291200       ADD +1 TO IX                                                       
291300     END-PERFORM                                                          
291400     EJECT                                                                
291500     .                                                                    
291600 S02-ADLAGOMR-KOLL  SECTION.                                              
291700                                                                          
291800     IF MSGI-IDUSER = 'PCSSE01'                                           
291900       IF WX-ADLAGOMR = 42 OR                                             
292000         (WY-ADLAGOMR = 42 AND WX-ADLAGOMR = 0) OR                        
292100          WX-ADLAGOMR = 43 OR                                             
292200         (WY-ADLAGOMR = 43 AND WX-ADLAGOMR = 0)                           
292300         CONTINUE                                                         
292400       ELSE                                                               
292500         MOVE NEJ        TO INDATA-SW                                     
292600                            ADLAGOMR-SW                                   
292700       END-IF                                                             
292800       IF WY-ADLAGOMR = 42 OR 43 OR 0                                     
292900         CONTINUE                                                         
293000       ELSE                                                               
293100         MOVE NEJ        TO INDATA-SW                                     
293200                            ADLAGOMR-SW                                   
293300       END-IF                                                             
293400     END-IF                                                               
293500     .                                                                    
293600     EJECT                                                                
293700 S03-BACKA-MOD  SECTION.                                                  
293800                                                                          
293900     MOVE MFS-ROER-EJ-FAELT    TO MOD-ADLAGOMR-CDC-OLD-ATTR               
294000                                  MOD-ADGANG-CDC-OLD-ATTR                 
294100                                  MOD-ADPLATS-CDC-OLD-ATTR                
294200                                  MOD-KVMAXPL-OLD-ATTR                    
294300*                                 MOD-ADLAGOMR-SVS-OLD-ATTR               
294400*                                 MOD-ADGANG-SVS-OLD-ATTR                 
294500*                                 MOD-ADPLATS-SVS-OLD-ATTR                
294600                                  MOD-ADLAGOMR-CD-OLD-ATTR(1)             
294700                                  MOD-ADLAGOMR-CD-OLD-ATTR(2)             
294800                                  MOD-ADLAGOMR-CD-OLD-ATTR(3)             
294900                                  MOD-ADLAGOMR-CD-OLD-ATTR(4)             
295000                                  MOD-ADGANG-CD-OLD-ATTR(1)               
295100                                  MOD-ADGANG-CD-OLD-ATTR(2)               
295200                                  MOD-ADGANG-CD-OLD-ATTR(3)               
295300                                  MOD-ADGANG-CD-OLD-ATTR(4)               
295400                                  MOD-ADPLATS-CD-OLD-ATTR(1)              
295500                                  MOD-ADPLATS-CD-OLD-ATTR(2)              
295600                                  MOD-ADPLATS-CD-OLD-ATTR(3)              
295700                                  MOD-ADPLATS-CD-OLD-ATTR(4)              
295800                                  MOD-VKART-OLD-ATTR                      
295900                                  MOD-VLARTNTO-OLD-ATTR                   
296000                                  MOD-KDVSOP-OLD-ATTR                     
296100                                  MOD-KDSPEEMB-OLD-ATTR                   
296200                                  MOD-IDARTNR-EMBQ3-OLD-ATTR              
296300                                  MOD-IDARTNR-EMBQ4-OLD-ATTR              
296400                                  MOD-FLEJBUFF-OLD-ATTR                   
296500                                  MOD-ADINLOMR-BOA-OLD-ATTR               
296600     .                                                                    
296700     EJECT                                                                
296800                                                                          
296900  MFS-ROER-EJ-BILD SECTION.                                               
297000                                                                          
297100     IF MID-ADLAGOMR-CDC NOT = ALL '+'                                    
297200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(1)                   
297300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(1)                        
297400     END-IF                                                               
297500                                                                          
297600     IF MID-ADGANG-CDC NOT = ALL '+'                                      
297700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(2)                   
297800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(2)                        
297900     END-IF                                                               
298000                                                                          
298100     IF MID-ADPLATS-CDC NOT = ALL '+'                                     
298200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(3)                   
298300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(3)                        
298400     END-IF                                                               
298500                                                                          
298600*    IF MID-ADLAGOMR-SVS NOT = ALL '+'                                    
298700*      MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(7)                   
298800*      MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(7)                        
298900*    END-IF                                                               
299000*                                                                         
299100*    IF MID-ADGANG-SVS NOT = ALL '+'                                      
299200*      MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(8)                   
299300*      MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(8)                        
299400*    END-IF                                                               
299500*                                                                         
299600*    IF MID-ADPLATS-SVS NOT = ALL '+'                                     
299700*      MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(9)                   
299800*      MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(9)                        
299900*    END-IF                                                               
300000                                                                          
300100     IF MID-ADGANG-CD(1) NOT = ALL '+'                                    
300200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(10)                  
300300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(10)                       
300400     END-IF                                                               
300500                                                                          
300600     IF MID-ADPLATS-CD(1) NOT = ALL '+'                                   
300700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(11)                  
300800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(11)                       
300900     END-IF                                                               
301000                                                                          
301100     IF MID-ADGANG-CD(2) NOT = ALL '+'                                    
301200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(12)                  
301300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(12)                       
301400     END-IF                                                               
301500                                                                          
301600     IF MID-ADPLATS-CD(2) NOT = ALL '+'                                   
301700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(13)                  
301800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(13)                       
301900     END-IF                                                               
302000                                                                          
302100     IF MID-ADGANG-CD(3) NOT = ALL '+'                                    
302200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(14)                  
302300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(14)                       
302400     END-IF                                                               
302500                                                                          
302600     IF MID-ADPLATS-CD(3) NOT = ALL '+'                                   
302700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(15)                  
302800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(15)                       
302900     END-IF                                                               
303000                                                                          
303100     IF MID-ADGANG-CD(4) NOT = ALL '+'                                    
303200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(16)                  
303300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(16)                       
303400     END-IF                                                               
303500                                                                          
303600     IF MID-ADPLATS-CD(4) NOT = ALL '+'                                   
303700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(17)                  
303800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(17)                       
303900     END-IF                                                               
304000                                                                          
305500                                                                          
305600     IF MID-KDSPEEMB NOT = ALL '+'                                        
305700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(21)                  
305800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(21)                       
305900     END-IF                                                               
306000                                                                          
306100     IF MID-IDARTNR-EMBQ3 NOT = ALL '+'                                   
306200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(22)                  
306300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(22)                       
306400     END-IF                                                               
306500                                                                          
306600     IF MID-IDARTNR-EMBQ4 NOT = ALL '+'                                   
306700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(23)                  
306800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(23)                       
306900     END-IF                                                               
307000                                                                          
307100     IF MID-FLEJBUFF NOT = ALL '+'                                        
307200       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(24)                  
307300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(24)                       
307400     END-IF                                                               
307500                                                                          
307600     IF MID-ADINLOMR-BOA  NOT = ALL '+'                                   
307700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(25)                  
307800       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(25)                       
307900     END-IF                                                               
308000                                                                          
308100     IF MID-KVMAXPL NOT = ALL '+'                                         
308200*      MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-NEW-ATTR(26)                  
308300       MOVE MFS-ROER-EJ-FAELT        TO MOD-NEW(26)                       
308400     END-IF                                                               
308500                                                                          
308600     IF MID-ADINLOMR-PRT  NOT = ALL '+'                                   
308700       MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-ADINLOMR-PRT-ATTR             
308800       MOVE MFS-ROER-EJ-FAELT        TO MOD-ADINLOMR-PRT                  
308900     END-IF                                                               
309000                                                                          
309100     EJECT                                                                
309200     .                                                                    
309300                                                                          
309400  MFS-RENSA-NYCKLAR SECTION.                                              
309500                                                                          
309600     MOVE MFS-RENSA-FAELT            TO MOD-IDARTNR-IN                    
309700                                        MOD-IDARTNR-UT                    
309800                                        MOD-STRECK                        
309900                                        MOD-REKSIFFR                      
310000                                                                          
310100     EJECT                                                                
310200     .                                                                    
310300                                                                          
310400  MFS-RENSA-BILDEN SECTION.                                               
310500                                                                          
310600     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(1)                   
310700     MOVE MFS-RENSA-FAELT            TO MOD-NEW(1)                        
310800                                                                          
310900     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(2)                   
311000     MOVE MFS-RENSA-FAELT            TO MOD-NEW(2)                        
311100                                                                          
311200     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(3)                   
311300     MOVE MFS-RENSA-FAELT            TO MOD-NEW(3)                        
311400                                                                          
311500     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(4)                   
311600     MOVE MFS-RENSA-FAELT            TO MOD-NEW(4)                        
311700                                                                          
311800     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(5)                   
311900     MOVE MFS-RENSA-FAELT            TO MOD-NEW(5)                        
312000                                                                          
312100     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(6)                   
312200     MOVE MFS-RENSA-FAELT            TO MOD-NEW(6)                        
312300                                                                          
312400     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(7)                   
312500     MOVE MFS-RENSA-FAELT            TO MOD-NEW(7)                        
312600                                                                          
312700     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(8)                   
312800     MOVE MFS-RENSA-FAELT            TO MOD-NEW(8)                        
312900                                                                          
313000     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(9)                   
313100     MOVE MFS-RENSA-FAELT            TO MOD-NEW(9)                        
313200                                                                          
313300     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(10)                  
313400     MOVE MFS-RENSA-FAELT            TO MOD-NEW(10)                       
313500                                                                          
313600     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(11)                  
313700     MOVE MFS-RENSA-FAELT            TO MOD-NEW(11)                       
313800                                                                          
313900     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(12)                  
314000     MOVE MFS-RENSA-FAELT            TO MOD-NEW(12)                       
314100                                                                          
314200     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(13)                  
314300     MOVE MFS-RENSA-FAELT            TO MOD-NEW(13)                       
314400                                                                          
314500     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(14)                  
314600     MOVE MFS-RENSA-FAELT            TO MOD-NEW(14)                       
314700                                                                          
314800     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(15)                  
314900     MOVE MFS-RENSA-FAELT            TO MOD-NEW(15)                       
315000                                                                          
315100     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(16)                  
315200     MOVE MFS-RENSA-FAELT            TO MOD-NEW(16)                       
315300                                                                          
315400     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(17)                  
315500     MOVE MFS-RENSA-FAELT            TO MOD-NEW(17)                       
315600                                                                          
315700     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(18)                  
315800     MOVE MFS-RENSA-FAELT            TO MOD-NEW(18)                       
315900                                                                          
316000     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(19)                  
316100     MOVE MFS-RENSA-FAELT            TO MOD-NEW(19)                       
316200                                                                          
316300     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(20)                  
316400     MOVE MFS-RENSA-FAELT            TO MOD-NEW(20)                       
316500                                                                          
316600     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(21)                  
316700     MOVE MFS-RENSA-FAELT            TO MOD-NEW(21)                       
316800                                                                          
316900     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(22)                  
317000     MOVE MFS-RENSA-FAELT            TO MOD-NEW(22)                       
317100                                                                          
317200     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(23)                  
317300     MOVE MFS-RENSA-FAELT            TO MOD-NEW(23)                       
317400                                                                          
317500     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(24)                  
317600     MOVE MFS-RENSA-FAELT            TO MOD-NEW(24)                       
317700                                                                          
317800     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(25)                  
317900     MOVE MFS-RENSA-FAELT            TO MOD-NEW(25)                       
318000                                                                          
318100     MOVE MFS-FORMATETS-ATTR         TO MOD-NEW-ATTR(26)                  
318200     MOVE MFS-RENSA-FAELT            TO MOD-NEW(26)                       
318300                                                                          
318400     MOVE MFS-FORMATETS-ATTR         TO MOD-ADINLOMR-PRT-ATTR             
318500     MOVE MFS-RENSA-FAELT            TO MOD-ADINLOMR-PRT                  
318600                                                                          
318700     EJECT                                                                
318800     .                                                                    
318900                                                                          
319000* IMS SEKTIONER                                                           
319100                                                                          
319200     SKIP3                                                                
319300 IMS-GET-MSG SECTION.                                                     
319400                                                                          
319500     MOVE '  QC' TO GODK-STATUSKODER                                      
319600     CALL CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                           
319700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
319800     PERFORM IMS-STATUS-KONTROLL                                          
319900     SKIP3                                                                
320000     .                                                                    
320100                                                                          
320200 IMS-ISRT-MSG SECTION.                                                    
320300                                                                          
320400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
320500       MOVE '0' TO MFS-KDHUVOMR                                           
320600     END-IF                                                               
320700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
320800     MOVE SPACE TO GODK-STATUSKODER                                       
320900     CALL CBLTDLI  USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD               
321000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
321100     PERFORM IMS-STATUS-KONTROLL                                          
321200     .                                                                    
321300     EJECT                                                                
321400                                                                          
321500 IMS-ISRT-ALT-MSG SECTION.                                                
321600                                                                          
321700     MOVE SPACE TO GODK-STATUSKODER                                       
321800     CALL  CBLTDLI  USING ISRT ALT-PCB P-TO-P-SW                          
321900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
322000     PERFORM IMS-STATUS-KONTROLL                                          
322100     .                                                                    
322200                                                                          
322300 IMS-PURG-4289    SECTION.                                                
322400                                                                          
322500     MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
322600     MOVE SPACE TO GODK-STATUSKODER                                       
322700     CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
322800     MOVE 4289-STATUS-CODE TO STATUS-WS                                   
322900     PERFORM IMS-STATUS-KONTROLL                                          
323000     .                                                                    
323100                                                                          
323200 IMS-GU-K601 SECTION.                                                     
323300                                                                          
323400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
323500             DELIMITED BY SIZE INTO SSA1                                  
323600     MOVE '  GE' TO GODK-STATUSKODER                                      
323700     CALL CBLTDLI  USING GU ARTC-PCB IO-AREA-2 SSA1                       
323800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
323900     PERFORM IMS-STATUS-KONTROLL                                          
324000     SKIP3                                                                
324100     .                                                                    
324200                                                                          
324300 IMS-GHNP-K611 SECTION.                                                   
324400                                                                          
324500     MOVE   'WLARTC11' TO SSA1                                            
324600     MOVE '  GE' TO GODK-STATUSKODER                                      
324700     CALL CBLTDLI USING GHNP ARTC-PCB IO-AREA-2 SSA1                      
324800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
324900     PERFORM IMS-STATUS-KONTROLL                                          
325000     EJECT                                                                
325100     .                                                                    
325200 IMS-GHU-WDK611 SECTION.                                                  
325300                                                                          
325400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR2-X ')'                        
325500             DELIMITED BY SIZE INTO SSA1                                  
325600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
325700             DELIMITED BY SIZE INTO SSA2                                  
325800     MOVE '  GE' TO GODK-STATUSKODER                                      
325900     CALL CBLTDLI  USING GHU WDK6-PCB IO-AREA-2 SSA1 SSA2                 
326000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
326100     PERFORM IMS-STATUS-KONTROLL                                          
326200     SKIP3                                                                
326300     .                                                                    
326400                                                                          
326500 IMS-REPL-WDK611 SECTION.                                                 
326600                                                                          
326700     MOVE '    ' TO GODK-STATUSKODER                                      
326800     CALL CBLTDLI USING REPL WDK6-PCB IO-AREA-2                           
326900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
327000     PERFORM IMS-STATUS-KONTROLL                                          
327100     .                                                                    
327200     EJECT                                                                
327300 IMS-GHU-WDK712 SECTION.                                                  
327400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
327500          DELIMITED BY SIZE INTO SSA1                                     
327600     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
327700          DELIMITED BY SIZE INTO SSA2                                     
328000     MOVE '  GE' TO GODK-STATUSKODER                                      
329000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK712                   
329100                                     SSA1 SSA2                            
329200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
329300     PERFORM IMS-STATUS-KONTROLL                                          
329400     .                                                                    
329500     EJECT                                                                
329600 IMS-REPL-WDK712 SECTION.                                                 
329700     MOVE '  ' TO GODK-STATUSKODER                                        
329800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK712                  
329900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
330000     PERFORM IMS-STATUS-KONTROLL                                          
330100     .                                                                    
330200     EJECT                                                                
330300 IMS-REPL-ARTC SECTION.                                                   
330400                                                                          
330500     MOVE '    ' TO GODK-STATUSKODER                                      
330600     CALL CBLTDLI USING REPL ARTC-PCB IO-AREA-2                           
330700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
330800     PERFORM IMS-STATUS-KONTROLL                                          
330900     .                                                                    
331000     EJECT                                                                
331100                                                                          
331200 IMS-GNP-ARTD11 SECTION.                                                  
331300                                                                          
331400     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
331500             DELIMITED BY SIZE INTO SSA1                                  
331600     MOVE '  GE' TO GODK-STATUSKODER                                      
331700     CALL CBLTDLI USING GNP ARTD-PCB IO-AREA-1 SSA1                       
331800     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
331900     PERFORM IMS-STATUS-KONTROLL                                          
332000                                                                          
332100     EJECT                                                                
332200     .                                                                    
332300                                                                          
332400 IMS-GU-ARTD01 SECTION.                                                   
332500                                                                          
332600     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
332700             DELIMITED BY SIZE INTO SSA1                                  
332800     MOVE '  GE' TO GODK-STATUSKODER                                      
332900     CALL CBLTDLI USING GU ARTD-PCB IO-AREA-1 SSA1                        
333000     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
333100     PERFORM IMS-STATUS-KONTROLL                                          
333200     EJECT                                                                
333300     .                                                                    
333400                                                                          
333500 IMS-GU-PLAA-6006 SECTION.                                                
333600                                                                          
333700     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-X ')'                         
333800             DELIMITED BY SIZE INTO SSA1                                  
333900     STRING 'W6PLAA11(ADINLOMR =' W-ADINLOMR-X ')'                        
334000             DELIMITED BY SIZE INTO SSA2                                  
334100     MOVE '  GE' TO GODK-STATUSKODER                                      
334200     CALL CBLTDLI USING GU PLAA-PCB IO-AREA-1 SSA1 SSA2                   
334300     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
334400     PERFORM IMS-STATUS-KONTROLL                                          
334500     EJECT                                                                
334600     .                                                                    
334700                                                                          
334800 IMS-GU-LOCB01 SECTION.                                                   
334900                                                                          
335000     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
335100          DELIMITED BY SIZE INTO SSA1                                     
335200     MOVE '  GE' TO GODK-STATUSKODER                                      
335300     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
335400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
335500     PERFORM IMS-STATUS-KONTROLL                                          
335600     .                                                                    
335700     EJECT                                                                
335800                                                                          
335900 IMS-ISRT-LOCB01 SECTION.                                                 
336000                                                                          
336100     MOVE 'WLLOCB01 ' TO SSA1                                             
336200     MOVE '  ' TO GODK-STATUSKODER                                        
336300     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB01 SSA1                
336400     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
336500     PERFORM IMS-STATUS-KONTROLL                                          
336600     .                                                                    
336700     SKIP3                                                                
336800                                                                          
336900 IMS-GHNP-LOCB11 SECTION.                                                 
337000                                                                          
337100     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
337200             DELIMITED BY SIZE INTO SSA1                                  
337300     MOVE '  GE' TO GODK-STATUSKODER                                      
337400     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                
337500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
337600     PERFORM IMS-STATUS-KONTROLL                                          
337700                                                                          
337800     EJECT                                                                
337900     .                                                                    
338000                                                                          
338100 IMS-REPL-LOCB11 SECTION.                                                 
338200                                                                          
338300     MOVE '  ' TO GODK-STATUSKODER                                        
338400     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-WLLOCB11                     
338500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
338600     PERFORM IMS-STATUS-KONTROLL                                          
338700     .                                                                    
338800     EJECT                                                                
338900     SKIP3                                                                
339000                                                                          
339100 IMS-ISRT-LOCB11 SECTION.                                                 
339200                                                                          
339300     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
339400          DELIMITED BY SIZE INTO SSA1                                     
339500     MOVE 'WLLOCB11 ' TO SSA2                                             
339600     MOVE '  II' TO GODK-STATUSKODER                                      
339700     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB11 SSA1 SSA2           
339800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
339900     PERFORM IMS-STATUS-KONTROLL                                          
340000     .                                                                    
340100     SKIP3                                                                
340200                                                                          
340300 IMS-STATUS-KONTROLL SECTION.                                             
340400                                                                          
340500     SET STATUS-IX TO 1                                                   
340600     SEARCH GODK-STATUS AT END CALL FELLOG                                
340700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
340800     END-SEARCH                                                           
340900     CONTINUE                                                             
341000     .                                                                    
