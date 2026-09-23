000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4066100.                                                
000400 AUTHOR.         MATS.                                                    
000500 DATE-WRITTEN.   OKT   85.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*    PROGRAMMET ÄR BÅDE ETT FRÅGEPROGRAM OCH ETT UPPDATERINGSPGM.         
001100*    FRÅGA:       ANGER VAD SOM FINNS UPPLAGT FÖR TRANSPORTEN.            
001200*    UPPDATERING: ANVÄNDS FÖR ATT LÄGGA TILL NY TRANSPORT, LÄGGA          
001300*                 TILL NYA DISTRIKT I TABELL UNDER EN BEFINTLIG           
001400*                 TRANSPORT, TA BORT DISTRIKT UR TABELLEN ELLER           
001500*                 ÄNDRA UPPGIFTER I TABELLEN.                             
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T661                                              
002000*        MID:         W4I66101                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W4O66101                                            
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77   PROGRAM-NAMN           VALUE 'W4066100'                             
003300                                 PIC X(8).                                
003400 77  JA                          PIC X(1)    VALUE 'J'.                   
003500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003600 77  YES                         PIC X(1)    VALUE 'Y'.                   
003700 77  WS-IDTRPTNR                 PIC X(3).                                
003800 77  WS-ADFLGEO                  PIC X(3).                                
003900 77  WS-ADFLOMR                  PIC X(3).                                
004000 77  WS-IDDC                     PIC X(2).                                
004100 77  WS-IDDC-CROSS               PIC X(2).                                
004200 77  WS-IDDC-SPAR                PIC X(2).                                
004300 77  WS-IDDISTR-FOM              PIC S9(4).                               
004400 77  WS-IDDISTR-TOM              PIC S9(4).                               
004500 77  WS-IDKUNDNR-FOM             PIC S9(6).                               
004600 77  WS-IDKUNDNR-TOM             PIC S9(6).                               
004700 77  JFR-IDDISTR                 PIC S9(5)   COMP-3.                      
004800 77  JFR-IDKUNDNR                PIC S9(7)   COMP-3.                      
004900 77  JFR-KDFRAKT                 PIC S9(3)   COMP-3.                      
005000 77  JFR-KDORDKLX                PIC X(1).                                
005100 77  JFR-IDDC-CROSS              PIC X(2).                                
005200 77  JFR-TEFLNOTE                PIC X(20).                               
005300 77  SPAR-ADRUTNIV               PIC S9(3)   COMP-3.                      
005400 77  INDX                        PIC S9(1)   COMP-3.                      
005500 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77  MAX-INTERVALL               PIC S9(9)   VALUE +20  COMP SYNC.        
005700 77  WS-INTERVALL                PIC S9(9)   VALUE +1   COMP SYNC.        
005800 77  MAX-RADER-PLUS              PIC S9(9)   VALUE +12  COMP SYNC.        
005900 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +805 COMP SYNC.        
006000 77  SW-INTERVALL                PIC X(1)    VALUE SPACE.                 
006100     88  DISTR-INTERVALL                     VALUE 'D'.                   
006200     88  KUND-INTERVALL                      VALUE 'K'.                   
006300 77  SW-ALLT-OK                  PIC X(1)    VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500 77  SW-NAGOT-IFYLLT             PIC X(1)    VALUE 'N'.                   
006600     88  NAGOT-IFYLLT                        VALUE 'J'.                   
006700 77  WS-IDTRANS                  PIC X(4).                                
006800     88  EGEN-BILD                           VALUE '4661'.                
006900     88  WS-GODKAEND-BILD                    VALUE '4661' '4662'.         
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500 01  NYCKLAR-TILL-DLI.                                                    
007600                                                                          
007700     03  W-4401-WDGXKEY-X.                                                
007800         05  FILLER              PIC X(4)    VALUE '4401'.                
007900         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008000                                                                          
008100     03  W-4402-KY4402-X.                                                 
008200         05  W-4402-IDDC         PIC X(02).                               
008300         05  W-4402-IDDISTR      PIC S9(5)   COMP-3.                      
008400         05  W-4402-IDKUNDNR     PIC S9(7)   COMP-3.                      
008500         05  W-4402-KDFRAKT      PIC S9(3)   COMP-3.                      
008600         05  W-4402-KDORDKLX     PIC X(1).                                
008700                                                                          
008800     03  W-4402-KY4402-MIN-X.                                             
008900         05  W-4402-IDDC-MIN     PIC X(02).                               
009000         05  W-4402-IDDISTR-MIN  PIC S9(5)   COMP-3.                      
009100         05  W-4402-IDKUNDNR-MIN PIC S9(7)   COMP-3.                      
009200         05  W-4402-KDFRAKT-MIN  PIC S9(3)   COMP-3.                      
009300         05  W-4402-KDORDKLX-MIN PIC X(1).                                
009400                                                                          
009500     03  W-4402-KY4402-HI-X.                                              
009600         05  W-4402-IDDC-HI      PIC X(02).                               
009700         05  FILLER              PIC X(10)   VALUE HIGH-VALUE.            
009800                                                                          
009900     03  W-4402-IDTRPTNR-X.                                               
010000         05  W-4402-IDTRPTNR     PIC S9(3)   COMP-3.                      
010100                                                                          
010200     03  W-4405-WDGXKEY-X.                                                
010300         05  FILLER              PIC X(4)    VALUE '4405'.                
010400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
010500                                                                          
010600     03  W-4406-WDGXKEY-X.                                                
010700         05  W-4406-IDTRPTNR     PIC S9(3)   COMP-3.                      
010800         05  W-4406-IDDC         PIC X(02).                               
010900         05  FILLER              PIC X(6)    VALUE LOW-VALUE.             
011000                                                                          
011100     03  W-4408-FLRUTFUL-X.                                               
011200         05  W-4408-FLRUTFUL     PIC X(1).                                
011300                                                                          
011400     03  W-4411-WDGXKEY-X.                                                
011500         05  FILLER              PIC X(4)    VALUE '4411'.                
011600         05  W-4411-ADCLGEO.                                              
011700             07  W-4411-IDDC     PIC X(02).                               
011800             07  W-4411-ADFLGEO  PIC X(3).                                
011900         05  FILLER              PIC X(21)   VALUE LOW-VALUE.             
012000                                                                          
012100     03  W-4412-WDGXKEY-X.                                                
012200         05  W-4412-ADFLOMR      PIC S9(3)   COMP-3.                      
012300         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
012400                                                                          
012500     03  W-4414-WDGXKEY-X.                                                
012600         05  W-4414-ADRUTNIV     PIC S9(3)   COMP-3.                      
012700         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
012800                                                                          
012900     03  W-4414-TESPAERR-X.                                               
013000         05  W-4414-TESPAERR     PIC X(20).                               
013100     EJECT                                                                
013200 01  W-TESPAERR.                                                          
013300*                                                                         
013400     03  FILLER                  PIC X(5)    VALUE 'TRPT '.               
013500     03  W-IDTRPTNR              PIC Z(2)9(1).                            
013600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
013700     03  W-DATE                  PIC 9(6).                                
013800     EJECT                                                                
013900                                                                          
014000 01  SAVE-AREA.                                                           
014100   03  SAVE-IDTRANS              PIC X(4)    VALUE SPACE.                 
014200   03  PGNO                      PIC 9(2)    VALUE 01.                    
014300   03  FIRST-SW                  PIC X       VALUE 'J'.                   
014400   03  SAVE-AREA-PREV OCCURS 20 TIMES.                                    
014500         05  SAVE-IDDISTR-PREV    PIC 9(4)   VALUE ZERO.                  
014600         05  SAVE-IDKUNDNR-PREV   PIC 9(6)   VALUE ZERO.                  
014700         05  SAVE-KDFRAKT-PREV    PIC 9(2)   VALUE ZERO.                  
014800         05  SAVE-KDORDKLX-PREV   PIC X      VALUE SPACE.                 
014900                                                                          
015000 01  FEL-MEDDELANDE.                                                      
015100*                                                                         
015200     03  W-FEL-1.                                                         
015300         05  FILLER              PIC X(40)   VALUE                        
015400             '749  FEL NYCKEL                         '.                  
015500         05  FILLER              PIC X(40)   VALUE                        
015600             '749  WRONG KEY                          '.                  
015700     03  FILLER REDEFINES W-FEL-1.                                        
015800         05  FEL-1 OCCURS 2      PIC X(40).                               
015900                                                                          
016000     03  W-FEL-2.                                                         
016100         05  FILLER              PIC X(40)   VALUE                        
016200             '748  UPPLYSTA FÄLT FEL                  '.                  
016300         05  FILLER              PIC X(40)   VALUE                        
016400             '748  HIGH-LIGHTED FIELDS WRONG          '.                  
016500     03  FILLER REDEFINES W-FEL-2.                                        
016600         05  FEL-2 OCCURS 2      PIC X(40).                               
016700                                                                          
016800     03  W-FEL-3.                                                         
016900         05  FILLER              PIC X(40)   VALUE                        
017000             '792  LASTNINGSOMRÅDE SAKNAS             '.                  
017100         05  FILLER              PIC X(40)   VALUE                        
017200             '792  LOADING AREA MISSING               '.                  
017300     03  FILLER REDEFINES W-FEL-3.                                        
017400         05  FEL-3 OCCURS 2      PIC X(40).                               
017500                                                                          
017600     03  W-FEL-4.                                                         
017700         05  FILLER              PIC X(40)   VALUE                        
017800             '793  GEOGRAFISKT OMRÅDE SAKNAS          '.                  
017900         05  FILLER              PIC X(40)   VALUE                        
018000             '793  GEOGRAPHIC AREA MISSING            '.                  
018100     03  FILLER REDEFINES W-FEL-4.                                        
018200         05  FEL-4 OCCURS 2      PIC X(40).                               
018300                                                                          
018400     03  W-FEL-5.                                                         
018500         05  FILLER              PIC X(40)   VALUE                        
018600             '794  DISTRIKT/KUND FINNS REDAN          '.                  
018700         05  FILLER              PIC X(40)   VALUE                        
018800             '794  DISTR/CUST ALREADY EXISTS          '.                  
018900     03  FILLER REDEFINES W-FEL-5.                                        
019000         05  FEL-5 OCCURS 2      PIC X(40).                               
019100                                                                          
019200     03  W-FEL-6.                                                         
019300         05  FILLER              PIC X(40)   VALUE                        
019400             '795  LEDIG RUTA SAKNAS                  '.                  
019500         05  FILLER              PIC X(40)   VALUE                        
019600             '795  FREE SQUARE MISSING                '.                  
019700     03  FILLER REDEFINES W-FEL-6.                                        
019800         05  FEL-6 OCCURS 2      PIC X(40).                               
019900                                                                          
020000     03  W-FEL-7.                                                         
020100         05  FILLER              PIC X(40)   VALUE                        
020200             '796  TRANSPORT SAKNAS                   '.                  
020300         05  FILLER              PIC X(40)   VALUE                        
020400             '796  TRANSPORT MISSING                  '.                  
020500     03  FILLER REDEFINES W-FEL-7.                                        
020600         05  FEL-7 OCCURS 2      PIC X(40).                               
020700                                                                          
020800     03  W-FEL-8.                                                         
020900         05  FILLER              PIC X(40)   VALUE                        
021000             '788  FRÅGA SKA STÄLLAS INNAN PF11       '.                  
021100         05  FILLER              PIC X(40)   VALUE                        
021200             '788  PF11 AND NEW KEYS NOT ALLOWED      '.                  
021300     03  FILLER REDEFINES W-FEL-8.                                        
021400         05  FEL-8 OCCURS 2      PIC X(40).                               
021500                                                                          
021600     03  W-FEL-9.                                                         
021700         05  FILLER              PIC X(40)   VALUE                        
021800             '812  TRYCK PF11 FÖR UPPDATERING         '.                  
021900         05  FILLER              PIC X(40)   VALUE                        
022000             '812  PRESS PF11 TO  UPDATE             '.                   
022100     03  FILLER REDEFINES W-FEL-9.                                        
022200         05  FEL-9 OCCURS 2       PIC X(40).                              
022300                                                                          
022400     03  W-FEL-10.                                                        
022500         05  FILLER              PIC X(40)   VALUE                        
022600             '789  INGET ÄNDRAT, UPPDATERING EJ UTFÖRD'.                  
022700         05  FILLER              PIC X(40)   VALUE                        
022800             '789  NOTHING CHANGED NO UPDATING MADE   '.                  
022900     03  FILLER REDEFINES W-FEL-10.                                       
023000         05  FEL-10 OCCURS 2       PIC X(40).                             
023100                                                                          
023200     03  W-FEL-11.                                                        
023300         05  FILLER              PIC X(40)   VALUE                        
023400             'FIRST PAGE'.                                                
023500         05  FILLER              PIC X(40)   VALUE                        
023600             'LAST PAGE'.                                                 
023700     03  FILLER REDEFINES W-FEL-11.                                       
023800         05  FEL-11 OCCURS 2       PIC X(40).                             
023900                                                                          
024000     EJECT                                                                
024100 01  INFO-MEDDELANDE.                                                     
024200*                                                                         
024300     03  W-INFO-1.                                                        
024400         05  FILLER              PIC X(40)   VALUE                        
024500             'UPPDATERING UTFÖRD                      '.                  
024600         05  FILLER              PIC X(40)   VALUE                        
024700             'UPDATING OK                             '.                  
024800     03  FILLER REDEFINES W-INFO-1.                                       
024900         05  INFO-1 OCCURS 2     PIC X(40).                               
025000                                                                          
025100     03  W-INFO-2.                                                        
025200         05  FILLER              PIC X(40)   VALUE                        
025300             'TRYCK PF8 FÖR NÄSTA SIDA                '.                  
025400         05  FILLER              PIC X(40)   VALUE                        
025500             'PRESS PF8 FOR NEXT PAGE                 '.                  
025600     03  FILLER REDEFINES W-INFO-2.                                       
025700         05  INFO-2 OCCURS 2     PIC X(40).                               
025800     EJECT                                                                
025900 01    FILLER              PIC X(16)   VALUE 'MFS-WS'.                    
026000     SKIP3                                                                
026100*01  -COPY WWDC99                                                         
026200     EJECT                                                                
026300 01    FILLER              PIC X(16)   VALUE 'MID W4I661 MID'.            
026400*01  MID -COPY W4I66101.                                                  
026500     EJECT                                                                
026600*01  -COPY WMSGAREA                                                       
026700     EJECT                                                                
026800*    03  MOD -COPY W4O66101  -RED MSG-AREA.                               
026900     EJECT                                                                
027000*01  -COPY WMFSAREA                                                       
027100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
027200*01 -COPY WMSGINIT                                                        
027300     EJECT                                                                
027400 01  IMS-WS.                                                              
027500     03  FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
027600     SKIP3                                                                
027700*                        **** STATUS-KOD FRÅN IMS                         
027800     03  STATUS-WS               PIC X(2).                                
027900         88  SEGMENT-FINNS                   VALUE '  '.                  
028000         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
028100     SKIP3                                                                
028200     03  GODK-STATUSKODER.                                                
028300         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
028400     SKIP3                                                                
028500 01  SSA1                        PIC X(96).                               
028600 01  SSA2                        PIC X(96).                               
028700 01  SSA3                        PIC X(96).                               
028800     EJECT                                                                
028900*                            IMS FUNKTIONSKODER                           
029000*01  -COPY W0003                                                          
029100     EJECT                                                                
029200*                            DLI INPUT-OUTPUT AREA                        
029300 01  DLI-IO-AREA.                                                         
029400     03  IO-AREA                 PIC X(128)  VALUE SPACE.                 
029500     SKIP2                                                                
029600*    03  WLXXDM11 -COPY WDGX4402   -RED IO-AREA.                          
029700     EJECT                                                                
029800*    03  WLXXDN11 -COPY WDGX4406   -RED IO-AREA.                          
029900     EJECT                                                                
030000*    03  WLXXDN21 -COPY WDGX4408   -RED IO-AREA.                          
030100     EJECT                                                                
030200*    03  WLXXDO01 -COPY WDGX4411   -RED IO-AREA.                          
030300     EJECT                                                                
030400*    03  WLXXDO11 -COPY WDGX4412   -RED IO-AREA.                          
030500     EJECT                                                                
030600*    03  WLXXDO21 -COPY WDGX4414   -RED IO-AREA.                          
030700     EJECT                                                                
030800 LINKAGE SECTION.                                                         
030900*01  -COPY W0009     -PRE MSG-                                            
031000     EJECT                                                                
031100*01  -COPY W0008     -PRE USEA-                                           
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008     -PRE XXDM-                                           
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700*01  -COPY W0008     -PRE XXDN-                                           
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008     -PRE XXDO-                                           
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB XXDM-PCB XXDN-PCB              
032400                          XXDO-PCB.                                       
032500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXDM-PCB XXDN-PCB             
032600                          XXDO-PCB.                                       
032700     SKIP2                                                                
032800 STYR SECTION.                                                            
032900     PERFORM IMS-GET-MSG                                                  
033000     IF SEGMENT-FINNS                                                     
033100         PERFORM A-INIT-SPARA-INPUT                                       
033200         IF WS-IDTRPTNR NUMERIC AND WS-ADFLOMR NUMERIC                    
033300* TRANSPORTNUMMER 999 ANVÄNDS SOM TRANSPORT VID UPPDATERING               
033400* AV WDE1 FÖR AUTOMATFAKTUROR OCH DIREKTLEVERANSER (DDI)                  
033500             IF MFS-UPDATE                                                
033600                 IF     MID-IDTRPTNR-IN = ALL '+'                         
033700                    AND MID-ADFLGEO-IN  = ALL '+'                         
033800                    AND MID-ADFLOMR-IN  = ALL '+'                         
033900                    AND MID-IDDC-IN     = ALL '+'                         
034000                     PERFORM B-KOLLA-INDATA                               
034100                     IF NAGOT-IFYLLT                                      
034200                         IF ALLT-OK                                       
034300                             PERFORM C-KOLLA-RELATION                     
034400                             IF ALLT-OK                                   
034500                                 PERFORM D-UPPDATERA                      
034600                             END-IF                                       
034700                         ELSE                                             
034800                             MOVE FEL-2 (INDX) TO MOD-TEMFSFEL            
034900                         END-IF                                           
035000                     ELSE                                                 
035100                         MOVE FEL-10 (INDX) TO MOD-TEMFSFEL               
035200                     END-IF                                               
035300                 ELSE                                                     
035400                     MOVE FEL-8 (INDX) TO MOD-TEMFSFEL                    
035500                     MOVE NEJ TO SW-ALLT-OK                               
035600                 END-IF                                                   
035700             ELSE                                                         
035800                 IF MID-NYUPPLAEGG NOT = ALL '+' AND EGEN-BILD            
035900                     MOVE NEJ TO SW-ALLT-OK                               
036000                     MOVE FEL-9 (INDX) TO MOD-TEMFSFEL                    
036100                 END-IF                                                   
036200             END-IF                                                       
036300             IF ALLT-OK                                                   
036400                 PERFORM E-FIXA-NYCKLAR                                   
036500                 PERFORM F-VISA-BILD                                      
036600             ELSE                                                         
036700                 PERFORM G-ROER-EJ-BILD                                   
036800             END-IF                                                       
036900         ELSE                                                             
037000             MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                            
037100         END-IF                                                           
037200         IF NOT WS-GODKAEND-BILD                                          
037300             PERFORM H-RENSA-NYCKLAR                                      
037400         END-IF                                                           
037500         PERFORM IMS-INSERT-MSG                                           
037600     END-IF                                                               
037700                                                                          
037800     MOVE ZERO TO RETURN-CODE                                             
037900     GOBACK                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 A-INIT-SPARA-INPUT SECTION.                                              
038300     SKIP2                                                                
038400     IF MSG-DUBBLA-TRANSKODER                                             
038500         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I66101               
038600         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                         
038700                                      WS-IDTRANS                          
038800         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
038900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
039000         MOVE MSG-IDPFK            TO MFS-IDPFK                           
039100     ELSE                                                                 
039200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I66101                
039300         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                         
039400                                      WS-IDTRANS                          
039500         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
039600         MOVE ' '                  TO MFS-KDTRTYP                         
039700                                      MFS-IDPFK                           
039800     END-IF                                                               
039900     MOVE MFS-IDTRANS              TO WS-IDTRANS                          
040000                                                                          
040100     IF MID-IDTRPTNR-IN = ALL '+'                                         
040200         MOVE MID-IDTRPTNR-UT      TO WS-IDTRPTNR                         
040300         INSPECT WS-IDTRPTNR REPLACING ALL SPACE BY ZERO                  
040400     ELSE                                                                 
040500         MOVE MID-IDTRPTNR-IN      TO WS-IDTRPTNR                         
040600     END-IF                                                               
040700                                                                          
040800     IF MID-ADFLGEO-IN = ALL '+'                                          
040900         MOVE MID-ADFLGEO-UT       TO WS-ADFLGEO                          
041000     ELSE                                                                 
041100         MOVE MID-ADFLGEO-IN       TO WS-ADFLGEO                          
041200     END-IF                                                               
041300                                                                          
041400     IF MID-ADFLOMR-IN = ALL '+'                                          
041500         MOVE MID-ADFLOMR-UT       TO WS-ADFLOMR                          
041600         INSPECT WS-ADFLOMR REPLACING ALL  SPACE BY ZERO                  
041700     ELSE                                                                 
041800         MOVE MID-ADFLOMR-IN       TO WS-ADFLOMR                          
041900     END-IF                                                               
042000                                                                          
042100     IF MID-IDDC-IN = ALL '+'                                             
042200         MOVE MID-IDDC-UT          TO WS-IDDC                             
042300     ELSE                                                                 
042400         MOVE MID-IDDC-IN          TO WS-IDDC                             
042500     END-IF                                                               
042600                                                                          
042700     MOVE LOW-VALUE                TO MSG-AREA                            
042800     MOVE 'W4O661N1'               TO MFS-IDMOD                           
042900     MOVE '4661'                   TO MOD-IDTRANS                         
043000     MOVE MAX-MOD-LAENGD           TO MSG-KVLL                            
043100                                                                          
043200     MOVE WS-IDTRPTNR              TO MOD-IDTRPTNR-UT                     
043300     INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE              
043400                                                                          
043500     MOVE WS-ADFLGEO               TO MOD-ADFLGEO-UT                      
043600                                                                          
043700     MOVE WS-ADFLOMR               TO MOD-ADFLOMR-UT                      
043800     INSPECT MOD-ADFLOMR-UT REPLACING LEADING ZERO BY SPACE               
043900     MOVE WS-IDDC                  TO MOD-IDDC-UT                         
044000                                                                          
044100     IF SWEDISH-TEXT                                                      
044200         MOVE +1 TO INDX                                                  
044300     ELSE                                                                 
044400         MOVE +2 TO INDX                                                  
044500     END-IF                                                               
044600                                                                          
044700     MOVE MFS-RENSA-FAELT TO MOD-IDTRPTNR-IN                              
044800                             MOD-ADFLGEO-IN                               
044900                             MOD-ADFLOMR-IN                               
045000                             MOD-IDDC-IN                                  
045100                             MOD-TEMFSFEL                                 
045200                             MOD-TEMFSINF                                 
045300                                                                          
045400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-B1                             
045500                               MOD-IDKUNDNR-B1                            
045600                               MOD-KDFRAKT-B1                             
045700                               MOD-KDORDKLX-B1                            
045800                               MOD-IDDISTR-BN                             
045900                               MOD-IDKUNDNR-BN                            
046000                               MOD-KDFRAKT-BN                             
046100                               MOD-KDORDKLX-BN                            
046200                                                                          
046300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046400     MOVE '001'             TO MSGI-KDCALL                                
046500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046600     MOVE '4661'            TO MSGI-IDTRANS                               
046700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
046800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046900     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
047000     .                                                                    
047100     EJECT                                                                
047200 B-KOLLA-INDATA SECTION.                                                  
047300     SKIP2                                                                
047400                                                                          
047500     MOVE NEJ TO SW-NAGOT-IFYLLT                                          
047600     MOVE JA TO SW-ALLT-OK                                                
047700     MOVE +1 TO RAD-IX                                                    
047800                                                                          
047900     PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                                
048000      IF MID-KDCMD (RAD-IX) NOT = ALL '+'                                 
048100       MOVE JA TO SW-NAGOT-IFYLLT                                         
048200       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD (RAD-IX)                       
048300       IF MID-KDCMD (RAD-IX) = 'B' OR 'Ä' OR 'C' OR 'D'                   
048400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (RAD-IX)              
048500       ELSE                                                               
048600        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (RAD-IX)                
048700        MOVE NEJ TO SW-ALLT-OK                                            
048800       END-IF                                                             
048900                                                                          
049000       INSPECT MID-IDDISTR-FOM (RAD-IX) REPLACING LEADING                 
049100                                        SPACE BY ZERO                     
049200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM (RAD-IX)                 
049300                                                                          
049400       IF MID-IDDISTR-TOM (RAD-IX) NOT = ALL '+'                          
049500        INSPECT MID-IDDISTR-TOM (RAD-IX) REPLACING LEADING                
049600                                         SPACE BY ZERO                    
049700        MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-TOM (RAD-IX)                
049800       ELSE                                                               
049900        MOVE MID-IDDISTR-FOM (RAD-IX) TO MID-IDDISTR-TOM (RAD-IX)         
050000        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM (RAD-IX)                  
050100       END-IF                                                             
050200                                                                          
050300       INSPECT MID-IDKUNDNR-FOM (RAD-IX) REPLACING LEADING                
050400                                         SPACE BY ZERO                    
050500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-FOM (RAD-IX)                
050600                                                                          
050700       IF MID-IDKUNDNR-TOM (RAD-IX) NOT = ALL '+'                         
050800        INSPECT MID-IDKUNDNR-TOM (RAD-IX) REPLACING LEADING               
050900                                          SPACE BY ZERO                   
051000        MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-TOM (RAD-IX)               
051100       ELSE                                                               
051200       MOVE MID-IDKUNDNR-FOM (RAD-IX) TO MID-IDKUNDNR-TOM (RAD-IX)        
051300        MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM (RAD-IX)                 
051400       END-IF                                                             
051500                                                                          
051600       INSPECT MID-KDFRAKT (RAD-IX) REPLACING LEADING                     
051700                                    SPACE BY ZERO                         
051800       MOVE MFS-ROER-EJ-FAELT TO MOD-KDFRAKT (RAD-IX)                     
051900                                                                          
052000       MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDKLX (RAD-IX)                    
052100                                                                          
052200       MOVE MFS-ROER-EJ-FAELT TO MOD-TEFLNOTE (RAD-IX)                    
052300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEFLNOTE-ATTR (RAD-IX)            
052400                                                                          
052500      END-IF                                                              
052600      ADD +1 TO RAD-IX                                                    
052700     END-PERFORM                                                          
052800                                                                          
052900     IF MID-NYUPPLAEGG NOT = ALL '+'                                      
053000         MOVE JA TO SW-NAGOT-IFYLLT                                       
053100         PERFORM BA-KOLLA-NYUPPLAEGG                                      
053200     END-IF                                                               
053300                                                                          
053400     IF MID-FLUTLAST = '+'                                                
053500         MOVE MFS-RENSA-FAELT TO MOD-FLUTLAST                             
053600     ELSE                                                                 
053700         MOVE JA TO SW-NAGOT-IFYLLT                                       
053800                                                                          
053900         MOVE MFS-ROER-EJ-FAELT TO MOD-FLUTLAST                           
054000         IF MID-FLUTLAST = JA OR NEJ OR YES                               
054100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLUTLAST-ATTR               
054200         ELSE                                                             
054300             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLUTLAST-ATTR                 
054400             MOVE NEJ TO SW-ALLT-OK                                       
054500         END-IF                                                           
054600     END-IF                                                               
054700                                                                          
054800     IF MID-FLTOTMS = '+'                                                 
054900         MOVE MFS-RENSA-FAELT TO MOD-FLTOTMS                              
055000     ELSE                                                                 
055100         MOVE JA TO SW-NAGOT-IFYLLT                                       
055200                                                                          
055300         MOVE MFS-ROER-EJ-FAELT TO MOD-FLTOTMS                            
055400         IF MID-FLTOTMS = JA OR NEJ OR YES                                
055500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTOTMS-ATTR                
055600         ELSE                                                             
055700             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTOTMS-ATTR                  
055800             MOVE NEJ TO SW-ALLT-OK                                       
055900         END-IF                                                           
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300 BA-KOLLA-NYUPPLAEGG SECTION.                                             
056400     SKIP2                                                                
056500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM-NY                         
056600                               MOD-IDDISTR-TOM-NY                         
056700                               MOD-IDKUNDNR-FOM-NY                        
056800                               MOD-IDKUNDNR-TOM-NY                        
056900                               MOD-KDFRAKT-NY                             
057000                               MOD-KDORDKLX-NY                            
057100                               MOD-IDDC-CROSS-NY                          
057200                               MOD-TEFLNOTE-NY                            
057300                                                                          
057400     IF MID-IDDISTR-FOM-NY NUMERIC                                        
057500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-NY-ATTR              
057600     ELSE                                                                 
057700         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-NY-ATTR                
057800         MOVE NEJ TO SW-ALLT-OK                                           
057900     END-IF                                                               
058000                                                                          
058100     IF MID-IDDISTR-TOM-NY = ALL '+'                                      
058200         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM-NY                       
058300     ELSE                                                                 
058400         IF MID-IDDISTR-TOM-NY NUMERIC                                    
058500             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-NY-ATTR          
058600         ELSE                                                             
058700             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-TOM-NY-ATTR            
058800             MOVE NEJ TO SW-ALLT-OK                                       
058900         END-IF                                                           
059000     END-IF                                                               
059100                                                                          
059200     IF MID-IDKUNDNR-FOM-NY = ALL '+'                                     
059300         MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-FOM-NY                      
059400     ELSE                                                                 
059500         IF MID-IDKUNDNR-FOM-NY NUMERIC                                   
059600             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-NY-ATTR         
059700         ELSE                                                             
059800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-NY-ATTR           
059900             MOVE NEJ TO SW-ALLT-OK                                       
060000         END-IF                                                           
060100     END-IF                                                               
060200                                                                          
060300     IF MID-IDKUNDNR-TOM-NY = ALL '+'                                     
060400         MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-TOM-NY                      
060500     ELSE                                                                 
060600         IF MID-IDKUNDNR-TOM-NY NUMERIC                                   
060700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-NY-ATTR         
060800         ELSE                                                             
060900             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-TOM-NY-ATTR           
061000             MOVE NEJ TO SW-ALLT-OK                                       
061100         END-IF                                                           
061200     END-IF                                                               
061300                                                                          
061400     IF MID-KDFRAKT-NY = ALL '+'                                          
061500         MOVE ZERO TO MID-KDFRAKT-NY                                      
061600         MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-NY                           
061700     ELSE                                                                 
061800         IF MID-KDFRAKT-NY NUMERIC                                        
061900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFRAKT-NY-ATTR              
062000         ELSE                                                             
062100             MOVE MFS-NUM-FAELT-FEL TO MOD-KDFRAKT-NY-ATTR                
062200             MOVE NEJ TO SW-ALLT-OK                                       
062300         END-IF                                                           
062400     END-IF                                                               
062500                                                                          
062600     IF MID-KDORDKLX-NY = ALL '+'                                         
062700         MOVE SPACE TO MID-KDORDKLX-NY                                    
062800         MOVE MFS-RENSA-FAELT TO MOD-KDORDKLX-NY                          
062900     ELSE                                                                 
063000         IF MID-KDORDKLX-NY = SPACE OR '0' OR '1' OR '2' OR '3'           
063100                 OR '4' OR '5'                                            
063200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDORDKLX-NY-ATTR            
063300         ELSE                                                             
063400             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDORDKLX-NY-ATTR              
063500             MOVE NEJ TO SW-ALLT-OK                                       
063600         END-IF                                                           
063700     END-IF                                                               
063800                                                                          
063900     IF MID-IDDC-CROSS-NY = ALL '+'                                       
064000*        MOVE SPACE TO MID-IDDC-CROSS-NY                                  
064100         MOVE MFS-RENSA-FAELT TO MOD-IDDC-CROSS-NY                        
064200     ELSE                                                                 
064300         IF MID-IDDC-CROSS-NY = SPACE                                     
064400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-CROSS-NY-ATTR          
064500         ELSE                                                             
064600             MOVE MID-IDDC-CROSS-NY TO WS-IDDC-CROSS                      
064700             MOVE WS-IDDC           TO WS-IDDC-SPAR                       
064800             MOVE WS-IDDC-CROSS TO WS-IDDC                                
064900             IF GOOD-DC                                                   
065000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-CROSS-NY-ATTR        
065100             ELSE                                                         
065200               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-CROSS-NY-ATTR          
065300               MOVE NEJ TO SW-ALLT-OK                                     
065400             END-IF                                                       
065500             MOVE WS-IDDC-SPAR      TO WS-IDDC                            
065600*        ELSE                                                             
065700*            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-CROSS-NY-ATTR            
065800*            MOVE NEJ TO SW-ALLT-OK                                       
065900         END-IF                                                           
066000     END-IF                                                               
066100                                                                          
066200     IF MID-TEFLNOTE-NY NOT = ALL '+'                                     
066300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEFLNOTE-NY-ATTR                
066400     ELSE                                                                 
066500         MOVE MFS-RENSA-FAELT TO MOD-TEFLNOTE-NY                          
066600     END-IF                                                               
066700                                                                          
066800     IF MID-IDDISTR-TOM-NY NOT = ALL '+' AND                              
066900             MID-IDKUNDNR-TOM-NY NOT = ALL '+'                            
067000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-TOM-NY-ATTR                
067100                                   MOD-IDKUNDNR-TOM-NY-ATTR               
067200         MOVE NEJ TO SW-ALLT-OK                                           
067300     END-IF                                                               
067400                                                                          
067500     IF ALLT-OK                                                           
067600         IF MID-IDDISTR-TOM-NY = ALL '+'                                  
067700             MOVE MID-IDDISTR-FOM-NY TO MID-IDDISTR-TOM-NY                
067800         END-IF                                                           
067900         IF MID-IDKUNDNR-FOM-NY = ALL '+'                                 
068000             MOVE ZERO TO MID-IDKUNDNR-FOM-NY                             
068100         END-IF                                                           
068200         IF MID-IDKUNDNR-TOM-NY = ALL '+'                                 
068300             MOVE MID-IDKUNDNR-FOM-NY TO MID-IDKUNDNR-TOM-NY              
068400         END-IF                                                           
068500                                                                          
068600         IF MID-IDDISTR-TOM-NY > MID-IDDISTR-FOM-NY + 19                  
068700             MOVE NEJ TO SW-ALLT-OK                                       
068800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-NY-ATTR            
068900                                       MOD-IDDISTR-TOM-NY-ATTR            
069000         END-IF                                                           
069100         IF MID-IDKUNDNR-TOM-NY > MID-IDKUNDNR-FOM-NY + 19                
069200             MOVE NEJ TO SW-ALLT-OK                                       
069300             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-NY-ATTR           
069400                                       MOD-IDKUNDNR-TOM-NY-ATTR           
069500         END-IF                                                           
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 C-KOLLA-RELATION SECTION.                                                
070000     SKIP2                                                                
070100     PERFORM IMS-GU-4405                                                  
070200     MOVE WS-IDTRPTNR      TO W-4406-IDTRPTNR                             
070300     MOVE WS-IDDC          TO W-4406-IDDC                                 
070400                              W-4411-IDDC                                 
070500     MOVE WS-ADFLGEO       TO W-4411-ADFLGEO                              
070600     MOVE WS-ADFLOMR       TO W-4412-ADFLOMR                              
070700     PERFORM IMS-GNP-4406                                                 
070800     IF SEGMENT-SAKNAS                                                    
070900         PERFORM IMS-GU-4411                                              
071000         IF SEGMENT-FINNS                                                 
071100             PERFORM IMS-GNP-4412                                         
071200             IF SEGMENT-FINNS                                             
071300                 MOVE SPACE TO W-4414-TESPAERR                            
071400                 PERFORM IMS-GHNP-4414                                    
071500                 IF SEGMENT-SAKNAS                                        
071600                     MOVE FEL-6 (INDX) TO MOD-TEMFSFEL                    
071700                     MOVE NEJ TO SW-ALLT-OK                               
071800                 ELSE                                                     
071900                     MOVE RUTA-ADRUTNIV TO W-4414-ADRUTNIV                
072000                 END-IF                                                   
072100             ELSE                                                         
072200                 MOVE FEL-3 (INDX) TO MOD-TEMFSFEL                        
072300                 MOVE NEJ TO SW-ALLT-OK                                   
072400             END-IF                                                       
072500         ELSE                                                             
072600             MOVE FEL-4 (INDX) TO MOD-TEMFSFEL                            
072700             MOVE NEJ TO SW-ALLT-OK                                       
072800         END-IF                                                           
072900     END-IF                                                               
073000                                                                          
073100     IF ALLT-OK AND MID-NYUPPLAEGG NOT = ALL '+'                          
073200         MOVE WS-IDDC            TO W-4402-IDDC                           
073300         MOVE MID-IDDISTR-FOM-NY TO WS-IDDISTR-FOM W-4402-IDDISTR         
073400         MOVE MID-IDDISTR-TOM-NY TO WS-IDDISTR-TOM                        
073500                                                                          
073600         MOVE MID-IDKUNDNR-FOM-NY TO WS-IDKUNDNR-FOM                      
073700         MOVE MID-IDKUNDNR-TOM-NY TO WS-IDKUNDNR-TOM                      
073800                                                                          
073900         MOVE MID-KDFRAKT-NY TO W-4402-KDFRAKT                            
074000                                                                          
074100         MOVE MID-KDORDKLX-NY TO W-4402-KDORDKLX                          
074200                                                                          
074300         PERFORM IMS-GU-4401                                              
074400         MOVE 'GE' TO STATUS-WS                                           
074500                                                                          
074600         PERFORM UNTIL W-4402-IDDISTR > WS-IDDISTR-TOM                    
074700                 OR SEGMENT-FINNS                                         
074800                                                                          
074900             MOVE WS-IDKUNDNR-FOM TO W-4402-IDKUNDNR                      
075000             PERFORM UNTIL W-4402-IDKUNDNR > WS-IDKUNDNR-TOM              
075100                     OR SEGMENT-FINNS                                     
075200                                                                          
075300                 PERFORM IMS-GHNP-4402                                    
075400                 ADD +1 TO W-4402-IDKUNDNR                                
075500             END-PERFORM                                                  
075600                                                                          
075700             ADD +1 TO W-4402-IDDISTR                                     
075800         END-PERFORM                                                      
075900         IF SEGMENT-FINNS                                                 
076000             MOVE NEJ TO SW-ALLT-OK                                       
076100             MOVE FEL-5 (INDX) TO MOD-TEMFSFEL                            
076200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-NY-ATTR            
076300                                       MOD-IDDISTR-TOM-NY-ATTR            
076400                                       MOD-IDKUNDNR-FOM-NY-ATTR           
076500                                       MOD-IDKUNDNR-TOM-NY-ATTR           
076600                                       MOD-KDFRAKT-NY-ATTR                
076700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDORDKLX-NY-ATTR              
076800                                        MOD-IDDC-CROSS-NY-ATTR            
076900                                        MOD-TEFLNOTE-NY-ATTR              
077000         END-IF                                                           
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 D-UPPDATERA SECTION.                                                     
077500     SKIP2                                                                
077600     MOVE +1 TO RAD-IX                                                    
077700     PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                                
077800         MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                       
077900         MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (RAD-IX)               
078000         IF MID-KDCMD-DELETE  (RAD-IX)                                    
078100             MOVE INFO-1 (INDX) TO MOD-TEMFSINF                           
078200             PERFORM DA-TAG-BORT-DISTRIKT                                 
078300         ELSE                                                             
078400             IF MID-KDCMD-REPLACE (RAD-IX)                                
078500                     AND MID-TEFLNOTE (RAD-IX) NOT = ALL '+'              
078600                 MOVE INFO-1 (INDX) TO MOD-TEMFSINF                       
078700                 PERFORM DB-AENDRA-TRANSPORT                              
078800             END-IF                                                       
078900         END-IF                                                           
079000         ADD +1 TO RAD-IX                                                 
079100     END-PERFORM                                                          
079200                                                                          
079300     IF MID-NYUPPLAEGG NOT = ALL '+'                                      
079400         MOVE INFO-1 (INDX) TO MOD-TEMFSINF                               
079500         PERFORM DC-NYTT-DISTRIKT                                         
079600     END-IF                                                               
079700                                                                          
079800     IF MID-FLUTLAST NOT = '+' OR MID-FLTOTMS NOT = '+'                   
079900         MOVE INFO-1 (INDX) TO MOD-TEMFSINF                               
080000         PERFORM DD-AENDRA-FLUTLAST-FLTOTMS                               
080100     END-IF                                                               
080200                                                                          
080300     PERFORM IMS-GU-4401                                                  
080400     MOVE WS-IDTRPTNR TO W-4402-IDTRPTNR                                  
080500                         W-4406-IDTRPTNR                                  
080600     MOVE WS-IDDC     TO W-4406-IDDC                                      
080610                         W-4402-IDDC-MIN                                  
080620                         W-4402-IDDC-HI                                   
080630                                                                          
080640     PERFORM IMS-GNP-4402-KVAL                                            
080800     IF SEGMENT-SAKNAS                                                    
080900         PERFORM IMS-GU-4411                                              
081000         PERFORM IMS-GU-4405                                              
081100         PERFORM IMS-GHNP-4406-FIRST                                      
081200         PERFORM IMS-GNP-4408-OKVAL                                       
081300         PERFORM UNTIL SEGMENT-SAKNAS                                     
081400             MOVE TRPTRUT-ADRUTNIV TO W-4414-ADRUTNIV                     
081500             PERFORM IMS-GHNP-4414-KVAL                                   
081600             PERFORM UNTIL SEGMENT-SAKNAS                                 
081700                 MOVE SPACE TO RUTA-TESPAERR                              
081800                 PERFORM IMS-REPL-XXDO                                    
081900                 PERFORM IMS-GHNP-4414-KVAL                               
082000             END-PERFORM                                                  
082100             PERFORM IMS-GNP-4408-OKVAL                                   
082200         END-PERFORM                                                      
082300                                                                          
082400         PERFORM IMS-GHNP-4406-FIRST                                      
082500         PERFORM IMS-DLET-XXDN                                            
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900 DA-TAG-BORT-DISTRIKT SECTION.                                            
083000     SKIP2                                                                
083100     MOVE WS-IDDC                  TO W-4402-IDDC                         
083200     MOVE MID-IDDISTR-FOM (RAD-IX) TO W-4402-IDDISTR                      
083300     MOVE MID-IDDISTR-TOM (RAD-IX) TO WS-IDDISTR-TOM                      
083400                                                                          
083500     MOVE MID-IDKUNDNR-FOM (RAD-IX) TO W-4402-IDKUNDNR                    
083600     MOVE MID-IDKUNDNR-TOM (RAD-IX) TO WS-IDKUNDNR-TOM                    
083700                                                                          
083800     MOVE MID-KDFRAKT (RAD-IX) TO W-4402-KDFRAKT                          
083900     MOVE MID-KDORDKLX (RAD-IX) TO W-4402-KDORDKLX                        
084000                                                                          
084100     PERFORM IMS-GU-4401                                                  
084200     PERFORM IMS-GHNP-4402                                                
084300                                                                          
084400     PERFORM UNTIL W-4402-IDDISTR > WS-IDDISTR-TOM                        
084500                   OR SEGMENT-SAKNAS                                      
084600                                                                          
084700         PERFORM UNTIL W-4402-IDKUNDNR > WS-IDKUNDNR-TOM                  
084800                       OR SEGMENT-SAKNAS                                  
084900                                                                          
085000             PERFORM IMS-DLET-XXDM                                        
085100             ADD +1 TO W-4402-IDKUNDNR                                    
085200             PERFORM IMS-GHNP-4402                                        
085300         END-PERFORM                                                      
085400                                                                          
085500         ADD +1 TO W-4402-IDDISTR                                         
085600         MOVE MID-IDKUNDNR-FOM (RAD-IX) TO W-4402-IDKUNDNR                
085700         PERFORM IMS-GHNP-4402                                            
085800     END-PERFORM                                                          
085900     .                                                                    
086000     EJECT                                                                
086100 DB-AENDRA-TRANSPORT SECTION.                                             
086200     SKIP2                                                                
086300     PERFORM IMS-GU-4401                                                  
086400     MOVE WS-IDDC                  TO W-4402-IDDC                         
086500     MOVE MID-IDDISTR-FOM (RAD-IX) TO W-4402-IDDISTR                      
086600     MOVE MID-IDDISTR-TOM (RAD-IX) TO WS-IDDISTR-TOM                      
086700                                                                          
086800     MOVE MID-IDKUNDNR-FOM (RAD-IX) TO W-4402-IDKUNDNR                    
086900     MOVE MID-IDKUNDNR-TOM (RAD-IX) TO WS-IDKUNDNR-TOM                    
087000                                                                          
087100     MOVE MID-KDFRAKT (RAD-IX) TO W-4402-KDFRAKT                          
087200     MOVE MID-KDORDKLX (RAD-IX) TO W-4402-KDORDKLX                        
087300                                                                          
087400     PERFORM IMS-GHNP-4402                                                
087500                                                                          
087600     PERFORM UNTIL W-4402-IDDISTR > WS-IDDISTR-TOM                        
087700                   OR SEGMENT-SAKNAS                                      
087800                                                                          
087900         PERFORM UNTIL W-4402-IDKUNDNR > WS-IDKUNDNR-TOM                  
088000                        OR SEGMENT-SAKNAS                                 
088100             MOVE MID-TEFLNOTE (RAD-IX) TO 4402-TEFLNOTE                  
088200             PERFORM IMS-REPL-XXDM                                        
088300             ADD +1 TO W-4402-IDKUNDNR                                    
088400             PERFORM IMS-GHNP-4402                                        
088500         END-PERFORM                                                      
088600                                                                          
088700         ADD +1 TO W-4402-IDDISTR                                         
088800         MOVE MID-IDKUNDNR-FOM (RAD-IX) TO W-4402-IDKUNDNR                
088900         PERFORM IMS-GHNP-4402                                            
089000     END-PERFORM                                                          
089100     .                                                                    
089200     EJECT                                                                
089300 DC-NYTT-DISTRIKT SECTION.                                                
089400     SKIP2                                                                
089500     PERFORM IMS-GU-4405                                                  
089600     MOVE WS-IDTRPTNR TO W-4406-IDTRPTNR                                  
089700     MOVE WS-IDDC     TO W-4406-IDDC                                      
089800     PERFORM IMS-GNP-4406                                                 
089900                                                                          
090000     IF SEGMENT-FINNS                                                     
090100                                                                          
090200         MOVE NEJ TO W-4408-FLRUTFUL                                      
090300         PERFORM IMS-GNP-4408                                             
090400         MOVE TRPTRUT-ADFLGEO TO MOD-ADFLGEO                              
090500         MOVE TRPTRUT-ADFLOMR TO MOD-ADFLOMR                              
090600         MOVE TRPTRUT-ADRUTNIV TO MOD-ADRUTNIV                            
090700         PERFORM DCA-ISRT-4402                                            
090800                                                                          
090900     ELSE                                                                 
091000                                                                          
091100         MOVE WS-IDDC    TO W-4411-IDDC                                   
091200         MOVE WS-ADFLGEO TO W-4411-ADFLGEO                                
091300         PERFORM IMS-GU-4411                                              
091400                                                                          
091500         MOVE WS-ADFLOMR TO W-4412-ADFLOMR                                
091600         PERFORM IMS-GNP-4412                                             
091700                                                                          
091800         MOVE SPACE TO W-4414-TESPAERR                                    
091900         PERFORM IMS-GHNP-4414                                            
092000                                                                          
092100         MOVE RUTA-ADRUTNIV TO SPAR-ADRUTNIV                              
092200         MOVE WS-IDTRPTNR TO W-IDTRPTNR                                   
092300         ACCEPT W-DATE FROM DATE                                          
092400         MOVE W-TESPAERR TO RUTA-TESPAERR                                 
092500         PERFORM IMS-REPL-XXDO                                            
092600         PERFORM DCA-ISRT-4402                                            
092700                                                                          
092800         MOVE SPACE TO DLI-IO-AREA                                        
092900                                                                          
093000         MOVE WS-IDTRPTNR TO TRPT-IDTRPTNR W-4406-IDTRPTNR                
093100         MOVE WS-IDDC     TO TRPT-IDDC     W-4406-IDDC                    
093200         MOVE LOW-VALUE   TO TRPT-LOWVALUE                                
093300         MOVE WS-ADFLGEO  TO TRPT-ADFLGEO                                 
093400         MOVE WS-ADFLOMR  TO TRPT-ADFLOMR                                 
093500         IF MID-FLUTLAST NOT = '+'                                        
093600             IF MID-FLUTLAST = YES                                        
093700               MOVE JA           TO TRPT-FLUTLAST                         
093800             ELSE                                                         
093900               MOVE MID-FLUTLAST TO TRPT-FLUTLAST                         
094000             END-IF                                                       
094100         ELSE                                                             
094200             MOVE JA TO TRPT-FLUTLAST                                     
094300         END-IF                                                           
094400         IF MID-FLTOTMS NOT = '+'                                         
094500             IF MID-FLTOTMS = YES                                         
094600               MOVE JA           TO TRPT-FLTOTMS                          
094700             ELSE                                                         
094800               MOVE MID-FLTOTMS  TO TRPT-FLTOTMS                          
094900             END-IF                                                       
095000         ELSE                                                             
095100             MOVE NEJ            TO TRPT-FLTOTMS                          
095200         END-IF                                                           
095300         PERFORM IMS-ISRT-4406                                            
095400                                                                          
095500         MOVE SPACE TO DLI-IO-AREA                                        
095600                                                                          
095700         MOVE WS-IDDC    TO TRPTRUT-IDDC                                  
095800         MOVE WS-ADFLGEO TO TRPTRUT-ADFLGEO                               
095900         MOVE WS-ADFLOMR TO TRPTRUT-ADFLOMR                               
096000         MOVE SPAR-ADRUTNIV TO TRPTRUT-ADRUTNIV                           
096100                            MOD-ADRUTNIV                                  
096200         MOVE LOW-VALUE TO TRPTRUT-LOWVALUE                               
096300         MOVE NEJ TO TRPTRUT-FLRUTFUL                                     
096400         PERFORM IMS-ISRT-4408                                            
096500                                                                          
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 DCA-ISRT-4402 SECTION.                                                   
097000     SKIP2                                                                
097100     MOVE SPACE TO DLI-IO-AREA                                            
097200                                                                          
097300     MOVE WS-IDDC             TO 4402-IDDC                                
097400                                                                          
097500     MOVE MID-IDDISTR-FOM-NY  TO 4402-IDDISTR                             
097600     MOVE MID-IDDISTR-TOM-NY  TO WS-IDDISTR-TOM                           
097700                                                                          
097800     MOVE MID-IDKUNDNR-FOM-NY TO 4402-IDKUNDNR                            
097900     MOVE MID-IDKUNDNR-TOM-NY TO WS-IDKUNDNR-TOM                          
098000                                                                          
098100     MOVE MID-KDFRAKT-NY      TO 4402-KDFRAKT                             
098200                                                                          
098300     MOVE MID-KDORDKLX-NY     TO 4402-KDORDKLX                            
098400                                                                          
098500     IF MID-IDDC-CROSS-NY NOT = ALL '+'                                   
098600       MOVE MID-IDDC-CROSS-NY TO 4402-IDDC-CROSS                          
098700     ELSE                                                                 
098800       MOVE SPACE             TO 4402-IDDC-CROSS                          
098900     END-IF                                                               
099000                                                                          
099100     IF MID-TEFLNOTE-NY NOT = ALL '+'                                     
099200         MOVE MID-TEFLNOTE-NY TO 4402-TEFLNOTE                            
099300     ELSE                                                                 
099400         MOVE SPACE TO 4402-TEFLNOTE                                      
099500     END-IF                                                               
099600                                                                          
099700     MOVE WS-IDTRPTNR TO 4402-IDTRPTNR                                    
099800     MOVE WS-IDDC     TO 4402-IDDC                                        
099900                                                                          
100000     PERFORM UNTIL 4402-IDDISTR > WS-IDDISTR-TOM                          
100100         MOVE MID-IDKUNDNR-FOM-NY TO 4402-IDKUNDNR                        
100200         PERFORM UNTIL 4402-IDKUNDNR > WS-IDKUNDNR-TOM                    
100300             PERFORM IMS-ISRT-4402                                        
100400             ADD +1 TO 4402-IDKUNDNR                                      
100500         END-PERFORM                                                      
100600         ADD +1 TO 4402-IDDISTR                                           
100700     END-PERFORM                                                          
100800     .                                                                    
100900     EJECT                                                                
101000 DD-AENDRA-FLUTLAST-FLTOTMS SECTION.                                      
101100     SKIP2                                                                
101200     PERFORM IMS-GU-4405                                                  
101300     MOVE WS-IDTRPTNR TO W-4406-IDTRPTNR                                  
101400     MOVE WS-IDDC     TO W-4406-IDDC                                      
101500     PERFORM IMS-GHNP-4406                                                
101600                                                                          
101700     IF SEGMENT-FINNS                                                     
101800       IF MID-FLUTLAST NOT = '+'                                          
101900         IF MID-FLUTLAST = YES                                            
102000           MOVE JA           TO TRPT-FLUTLAST                             
102100         ELSE                                                             
102200           MOVE MID-FLUTLAST TO TRPT-FLUTLAST                             
102300         END-IF                                                           
102400       END-IF                                                             
102500       IF MID-FLTOTMS NOT = '+'                                           
102600         IF MID-FLTOTMS = YES                                             
102700           MOVE JA           TO TRPT-FLTOTMS                              
102800         ELSE                                                             
102900           MOVE MID-FLTOTMS  TO TRPT-FLTOTMS                              
103000         END-IF                                                           
103100       END-IF                                                             
103200       PERFORM IMS-REPL-XXDN                                              
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600 E-FIXA-NYCKLAR SECTION.                                                  
103700     SKIP2                                                                
103800     MOVE WS-IDTRPTNR TO W-4402-IDTRPTNR                                  
103900     MOVE WS-IDDC     TO W-4402-IDDC                                      
104000                         W-4402-IDDC-HI                                   
104100                         W-4402-IDDC-MIN                                  
104200                                                                          
104300     EVALUATE TRUE                                                        
104400       WHEN MFS-PREVIOUS                                                  
104500       IF SAVE-IDTRANS = '4661'                                           
104600         IF PGNO > 1                                                      
104700           COMPUTE PGNO = PGNO - 1                                        
104800         END-IF                                                           
104900         IF PGNO = 1                                                      
105000           IF FIRST-SW = JA                                               
105100              MOVE FEL-11 (1)            TO MOD-TEMFSFEL                  
105200           END-IF                                                         
105300         END-IF                                                           
105400         MOVE SAVE-IDDISTR-PREV(PGNO)    TO W-4402-IDDISTR                
105500                                          W-4402-IDDISTR-MIN              
105600         MOVE SAVE-IDKUNDNR-PREV(PGNO) TO W-4402-IDKUNDNR                 
105700                                          W-4402-IDKUNDNR-MIN             
105800         MOVE SAVE-KDFRAKT-PREV(PGNO)    TO W-4402-KDFRAKT                
105900                                          W-4402-KDFRAKT-MIN              
106000         MOVE SAVE-KDORDKLX-PREV(PGNO) TO W-4402-KDORDKLX                 
106100                                          W-4402-KDORDKLX-MIN             
106200       END-IF                                                             
106300                                                                          
106400       WHEN MFS-FIRST OR MID-IDTRPTNR-IN NOT = ALL '+'                    
106500               OR NOT EGEN-BILD                                           
106600           MOVE 1                      TO PGNO                            
106700           MOVE JA                     TO FIRST-SW                        
106800           MOVE FEL-11 (1)             TO MOD-TEMFSFEL                    
106900           MOVE ZERO TO W-4402-IDDISTR W-4402-IDDISTR-MIN                 
107000                        W-4402-IDKUNDNR W-4402-IDKUNDNR-MIN               
107100                        W-4402-KDFRAKT W-4402-KDFRAKT-MIN                 
107200           MOVE SPACE TO W-4402-KDORDKLX W-4402-KDORDKLX-MIN              
107300                                                                          
107400       WHEN MFS-NEXT                                                      
107500       IF SAVE-IDTRANS = '4661'                                           
107600           IF  MID-IDDISTR-BN   = SAVE-IDDISTR-PREV(PGNO)  AND            
107700               MID-IDKUNDNR-BN  = SAVE-IDKUNDNR-PREV(PGNO) AND            
107800               MID-KDFRAKT-BN   = SAVE-KDFRAKT-PREV(PGNO)  AND            
107900               MID-KDORDKLX-BN  = SAVE-KDORDKLX-PREV(PGNO)                
108000             CONTINUE                                                     
108100           ELSE                                                           
108200             IF PGNO = 20                                                 
108300               PERFORM VARYING PGNO FROM 1 BY 1 UNTIL PGNO = 20           
108400               MOVE SAVE-IDDISTR-PREV(PGNO + 1)  TO                       
108500                                 SAVE-IDDISTR-PREV(PGNO)                  
108600               MOVE SAVE-IDKUNDNR-PREV(PGNO + 1) TO                       
108700                                 SAVE-IDKUNDNR-PREV(PGNO)                 
108800               MOVE SAVE-KDFRAKT-PREV(PGNO + 1)  TO                       
108900                                 SAVE-KDFRAKT-PREV(PGNO)                  
109000               MOVE SAVE-KDORDKLX-PREV(PGNO + 1) TO                       
109100                                 SAVE-KDORDKLX-PREV(PGNO)                 
109200               END-PERFORM                                                
109300               MOVE NEJ TO FIRST-SW                                       
109400             ELSE                                                         
109500               COMPUTE PGNO = PGNO + 1                                    
109600             END-IF                                                       
109700           END-IF                                                         
109800           MOVE MID-IDDISTR-BN TO W-4402-IDDISTR                          
109900                                  W-4402-IDDISTR-MIN                      
110000           MOVE MID-IDKUNDNR-BN TO W-4402-IDKUNDNR                        
110100                                   W-4402-IDKUNDNR-MIN                    
110200           MOVE MID-KDFRAKT-BN TO W-4402-KDFRAKT                          
110300                                  W-4402-KDFRAKT-MIN                      
110400           MOVE MID-KDORDKLX-BN TO W-4402-KDORDKLX                        
110500                                   W-4402-KDORDKLX-MIN                    
110600       END-IF                                                             
110700                                                                          
110800       WHEN OTHER                                                         
110900           MOVE MID-IDDISTR-B1 TO W-4402-IDDISTR                          
111000                                  W-4402-IDDISTR-MIN                      
111100           MOVE MID-IDKUNDNR-B1 TO W-4402-IDKUNDNR                        
111200                                   W-4402-IDKUNDNR-MIN                    
111300           MOVE MID-KDFRAKT-B1 TO W-4402-KDFRAKT                          
111400                                  W-4402-KDFRAKT-MIN                      
111500           MOVE MID-KDORDKLX-B1 TO W-4402-KDORDKLX                        
111600                                   W-4402-KDORDKLX-MIN                    
111700     END-EVALUATE                                                         
111800     .                                                                    
111900     EJECT                                                                
112000 F-VISA-BILD SECTION.                                                     
112100     SKIP2                                                                
112200     PERFORM IMS-GU-4405                                                  
112300                                                                          
112400     MOVE WS-IDTRPTNR TO W-4406-IDTRPTNR                                  
112500     MOVE WS-IDDC     TO W-4406-IDDC                                      
112600                                                                          
112700     MOVE +1 TO RAD-IX                                                    
112800     PERFORM IMS-GNP-4406                                                 
112900                                                                          
113000     IF SEGMENT-FINNS                                                     
113100       IF TRPT-FLUTLAST = JA AND ENGLISH-TEXT                             
113200         MOVE YES             TO MOD-FLUTLAST                             
113300       ELSE                                                               
113400         MOVE TRPT-FLUTLAST   TO MOD-FLUTLAST                             
113500       END-IF                                                             
113600                                                                          
113700       IF TRPT-FLTOTMS = JA AND ENGLISH-TEXT                              
113800         MOVE YES             TO MOD-FLTOTMS                              
113900       ELSE                                                               
114000         MOVE TRPT-FLTOTMS    TO MOD-FLTOTMS                              
114100       END-IF                                                             
114200                                                                          
114300       PERFORM IMS-GU-4401                                                
114400       PERFORM IMS-GNP-4402-KVAL                                          
114500       IF SEGMENT-FINNS                                                   
114600         MOVE 4402-IDDISTR     TO MOD-IDDISTR-B1                          
114700                                  SAVE-IDDISTR-PREV(PGNO)                 
114800         MOVE 4402-IDKUNDNR TO MOD-IDKUNDNR-B1                            
114900                                  SAVE-IDKUNDNR-PREV(PGNO)                
115000         MOVE 4402-KDFRAKT     TO MOD-KDFRAKT-B1                          
115100                                  SAVE-KDFRAKT-PREV(PGNO)                 
115200         MOVE 4402-KDORDKLX TO MOD-KDORDKLX-B1                            
115300                                  SAVE-KDORDKLX-PREV(PGNO)                
115400         PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                            
115500                       OR SEGMENT-SAKNAS                                  
115600                                                                          
115700           PERFORM FA-FLYTTA-INITIAL-VAERDEN                              
115800           PERFORM IMS-GNP-4402-KVAL                                      
115900           PERFORM FB-TESTA-OM-INTERVALL                                  
116000                                                                          
116100           PERFORM UNTIL NOT (DISTR-INTERVALL OR KUND-INTERVALL)          
116200                         OR SEGMENT-SAKNAS                                
116300                                                                          
116400             IF KUND-INTERVALL                                            
116500               MOVE 4402-IDKUNDNR TO MOD-IDKUNDNR-TOM (RAD-IX)            
116600                                        JFR-IDKUNDNR                      
116700             ELSE                                                         
116800               MOVE 4402-IDDISTR TO MOD-IDDISTR-TOM (RAD-IX)              
116900                                       JFR-IDDISTR                        
117000             END-IF                                                       
117100             PERFORM IMS-GNP-4402-KVAL                                    
117200             PERFORM FB-TESTA-OM-INTERVALL                                
117300           END-PERFORM                                                    
117400           ADD +1 TO RAD-IX                                               
117500         END-PERFORM                                                      
117600                                                                          
117700         IF SEGMENT-FINNS                                                 
117800             MOVE 4402-IDDISTR TO MOD-IDDISTR-BN                          
117900             MOVE 4402-IDKUNDNR TO MOD-IDKUNDNR-BN                        
118000             MOVE 4402-KDFRAKT TO MOD-KDFRAKT-BN                          
118100             MOVE 4402-KDORDKLX TO MOD-KDORDKLX-BN                        
118200             IF NOT MFS-UPDATE                                            
118300                 MOVE INFO-2 (INDX) TO MOD-TEMFSINF                       
118400             END-IF                                                       
118500         ELSE                                                             
118600             MOVE FEL-11 (2)        TO MOD-TEMFSFEL                       
118700         END-IF                                                           
118800                                                                          
118900                                                                          
119000         MOVE NEJ TO W-4408-FLRUTFUL                                      
119100         PERFORM IMS-GNP-4408                                             
119200                                                                          
119300         MOVE TRPTRUT-ADFLGEO TO MOD-ADFLGEO MOD-ADFLGEO-UT               
119400         MOVE TRPTRUT-ADFLOMR TO MOD-ADFLOMR MOD-ADFLOMR-UT               
119500         INSPECT MOD-ADFLOMR-UT REPLACING LEADING ZERO BY SPACE           
119600         MOVE TRPTRUT-ADRUTNIV TO MOD-ADRUTNIV                            
119700       END-IF                                                             
119800     ELSE                                                                 
119900       MOVE FEL-7 (INDX) TO MOD-TEMFSFEL                                  
120000     END-IF                                                               
120100       MOVE '002'     TO MSGI-KDCALL                                      
120200       MOVE '4661'    TO MSGI-IDTRANS                                     
120300       MOVE '4661'    TO SAVE-IDTRANS                                     
120400       MOVE SAVE-AREA TO MSGI-SPAR-AREA                                   
120500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
120600                                                                          
120700     PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                                
120800         MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                       
120900                                 MOD-IDDISTR-FOM (RAD-IX)                 
121000                                 MOD-IDDISTR-TOM (RAD-IX)                 
121100                                 MOD-IDKUNDNR-FOM (RAD-IX)                
121200                                 MOD-IDKUNDNR-TOM (RAD-IX)                
121300                                 MOD-KDFRAKT (RAD-IX)                     
121400                                 MOD-KDORDKLX (RAD-IX)                    
121500                                 MOD-IDDC-CROSS (RAD-IX)                  
121600                                 MOD-TEFLNOTE (RAD-IX)                    
121700         MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (RAD-IX)               
121800                                    MOD-TEFLNOTE-ATTR (RAD-IX)            
121900         ADD +1 TO RAD-IX                                                 
122000     END-PERFORM                                                          
122100                                                                          
122200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM-NY                           
122300                             MOD-IDDISTR-TOM-NY                           
122400                             MOD-IDKUNDNR-FOM-NY                          
122500                             MOD-IDKUNDNR-TOM-NY                          
122600                             MOD-KDFRAKT-NY                               
122700                             MOD-KDORDKLX-NY                              
122800                             MOD-IDDC-CROSS-NY                            
122900                             MOD-TEFLNOTE-NY                              
123000     .                                                                    
123100     EJECT                                                                
123200 FA-FLYTTA-INITIAL-VAERDEN SECTION.                                       
123300     SKIP2                                                                
123400     MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-KDCMD-ATTR (RAD-IX)                
123500                                   MOD-TEFLNOTE-ATTR (RAD-IX)             
123600     MOVE 4402-IDDISTR TO MOD-IDDISTR-FOM (RAD-IX)                        
123700                             JFR-IDDISTR                                  
123800     MOVE 4402-IDKUNDNR TO MOD-IDKUNDNR-FOM (RAD-IX)                      
123900                              JFR-IDKUNDNR                                
124000     MOVE 4402-KDFRAKT TO MOD-KDFRAKT (RAD-IX)                            
124100                             JFR-KDFRAKT                                  
124200     MOVE 4402-KDORDKLX TO MOD-KDORDKLX (RAD-IX)                          
124300                              JFR-KDORDKLX                                
124400     MOVE 4402-IDDC-CROSS TO MOD-IDDC-CROSS (RAD-IX)                      
124500                              JFR-IDDC-CROSS                              
124600     MOVE 4402-TEFLNOTE TO MOD-TEFLNOTE (RAD-IX)                          
124700                              JFR-TEFLNOTE                                
124800                                                                          
124900     MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                           
125000                             MOD-IDDISTR-TOM (RAD-IX)                     
125100                             MOD-IDKUNDNR-TOM (RAD-IX)                    
125200     .                                                                    
125300     EJECT                                                                
125400 FB-TESTA-OM-INTERVALL SECTION.                                           
125500     SKIP2                                                                
125600     IF NOT DISTR-INTERVALL AND WS-INTERVALL < MAX-INTERVALL              
125700             AND 4402-IDDISTR = JFR-IDDISTR                               
125800             AND 4402-IDKUNDNR = JFR-IDKUNDNR + 1                         
125900             AND 4402-KDFRAKT = JFR-KDFRAKT                               
126000             AND 4402-KDORDKLX = JFR-KDORDKLX                             
126100             AND 4402-IDDC-CROSS = JFR-IDDC-CROSS                         
126200             AND 4402-TEFLNOTE = JFR-TEFLNOTE                             
126300         ADD +1 TO WS-INTERVALL                                           
126400         MOVE 'K' TO SW-INTERVALL                                         
126500                                                                          
126600     ELSE                                                                 
126700         IF NOT KUND-INTERVALL AND WS-INTERVALL < MAX-INTERVALL           
126800                 AND 4402-IDDISTR = JFR-IDDISTR + 1                       
126900                 AND 4402-IDKUNDNR = JFR-IDKUNDNR                         
127000                 AND 4402-KDFRAKT = JFR-KDFRAKT                           
127100                 AND 4402-KDORDKLX = JFR-KDORDKLX                         
127200                 AND 4402-IDDC-CROSS = JFR-IDDC-CROSS                     
127300                 AND 4402-TEFLNOTE = JFR-TEFLNOTE                         
127400             ADD +1 TO WS-INTERVALL                                       
127500             MOVE 'D' TO SW-INTERVALL                                     
127600                                                                          
127700         ELSE                                                             
127800             MOVE +1 TO WS-INTERVALL                                      
127900             MOVE SPACE TO SW-INTERVALL                                   
128000         END-IF                                                           
128100     END-IF                                                               
128200     .                                                                    
128300     EJECT                                                                
128400 G-ROER-EJ-BILD SECTION.                                                  
128500     SKIP2                                                                
128600     MOVE MFS-ROER-EJ-FAELT TO MOD-ADFLGEO                                
128700                               MOD-ADFLOMR                                
128800                               MOD-ADRUTNIV                               
128900                                                                          
129000     MOVE +1 TO RAD-IX                                                    
129100     PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                                
129200         MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM (RAD-IX)               
129300                                   MOD-IDDISTR-TOM (RAD-IX)               
129400                                   MOD-IDKUNDNR-FOM (RAD-IX)              
129500                                   MOD-IDKUNDNR-TOM (RAD-IX)              
129600                                   MOD-KDFRAKT (RAD-IX)                   
129700                                   MOD-KDORDKLX (RAD-IX)                  
129800                                   MOD-IDDC-CROSS (RAD-IX)                
129900                                   MOD-TEFLNOTE (RAD-IX)                  
130000         ADD +1 TO RAD-IX                                                 
130100     END-PERFORM                                                          
130200                                                                          
130300     MOVE MFS-ROER-EJ-FAELT TO MOD-FLUTLAST                               
130400                               MOD-FLTOTMS                                
130500                               MOD-IDDISTR-FOM-NY                         
130600                               MOD-IDDISTR-TOM-NY                         
130700                               MOD-IDKUNDNR-FOM-NY                        
130800                               MOD-IDKUNDNR-TOM-NY                        
130900                               MOD-KDFRAKT-NY                             
131000                               MOD-KDORDKLX-NY                            
131100                               MOD-IDDC-CROSS-NY                          
131200                               MOD-TEFLNOTE-NY                            
131300                                                                          
131400     IF NOT MFS-UPDATE                                                    
131500         MOVE +1 TO RAD-IX                                                
131600         PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                            
131700           IF MID-KDCMD (RAD-IX) NOT = ALL '+'                            
131800             MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (RAD-IX)        
131900                                        MOD-TEFLNOTE-ATTR (RAD-IX)        
132000           END-IF                                                         
132100           ADD +1 TO RAD-IX                                               
132200         END-PERFORM                                                      
132300         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLUTLAST-ATTR                  
132400                                       MOD-FLTOTMS-ATTR                   
132500                                       MOD-IDDISTR-FOM-NY-ATTR            
132600                                       MOD-IDDISTR-TOM-NY-ATTR            
132700                                       MOD-IDKUNDNR-FOM-NY-ATTR           
132800                                       MOD-IDKUNDNR-TOM-NY-ATTR           
132900                                       MOD-KDFRAKT-NY-ATTR                
133000                                       MOD-KDORDKLX-NY-ATTR               
133100                                       MOD-IDDC-CROSS-NY-ATTR             
133200                                       MOD-TEFLNOTE-NY-ATTR               
133300     ELSE                                                                 
133400         IF MID-IDTRPTNR-IN NOT = ALL '+'                                 
133500                 OR MID-ADFLGEO-IN NOT = ALL '+'                          
133600                 OR MID-ADFLOMR-IN NOT = ALL '+'                          
133700             MOVE +1 TO RAD-IX                                            
133800             PERFORM UNTIL RAD-IX > MAX-RADER-PLUS                        
133900                MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (RAD-IX)        
134000                                        MOD-TEFLNOTE-ATTR (RAD-IX)        
134100                MOVE MFS-RENSA-FAELT TO MOD-KDCMD (RAD-IX)                
134200                                        MOD-TEFLNOTE (RAD-IX)             
134300                ADD +1 TO RAD-IX                                          
134400             END-PERFORM                                                  
134500             MOVE MFS-FORMATETS-ATTR TO MOD-FLUTLAST-ATTR                 
134600                                        MOD-FLTOTMS-ATTR                  
134700                                        MOD-IDDISTR-FOM-NY-ATTR           
134800                                        MOD-IDDISTR-TOM-NY-ATTR           
134900                                        MOD-IDKUNDNR-FOM-NY-ATTR          
135000                                        MOD-IDKUNDNR-TOM-NY-ATTR          
135100                                        MOD-KDFRAKT-NY-ATTR               
135200                                        MOD-KDORDKLX-NY-ATTR              
135300                                        MOD-IDDC-CROSS-NY-ATTR            
135400                                        MOD-TEFLNOTE-NY-ATTR              
135500             MOVE MFS-RENSA-FAELT TO MOD-FLUTLAST                         
135600                                     MOD-FLTOTMS                          
135700                                     MOD-IDDISTR-FOM-NY                   
135800                                     MOD-IDDISTR-TOM-NY                   
135900                                     MOD-IDKUNDNR-FOM-NY                  
136000                                     MOD-IDKUNDNR-TOM-NY                  
136100                                     MOD-KDFRAKT-NY                       
136200                                     MOD-KDORDKLX-NY                      
136300                                     MOD-IDDC-CROSS-NY                    
136400                                     MOD-TEFLNOTE-NY                      
136500         END-IF                                                           
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900 H-RENSA-NYCKLAR SECTION.                                                 
137000     MOVE MFS-RENSA-FAELT            TO MOD-IDTRPTNR-UT                   
137100                                        MOD-ADFLGEO-UT                    
137200                                        MOD-ADFLOMR-UT                    
137300     EJECT                                                                
137400* IMS SEKTIONER                                                           
137500     SKIP3                                                                
137600     .                                                                    
137700 IMS-GET-MSG SECTION.                                                     
137800     SKIP2                                                                
137900     MOVE '  QC' TO GODK-STATUSKODER                                      
138000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
138100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     SKIP3                                                                
138400     .                                                                    
138500 IMS-INSERT-MSG SECTION.                                                  
138600     SKIP2                                                                
138700     IF NOT ENGLISH-TEXT                                                  
138800         MOVE '0' TO MFS-KDHUVOMR                                         
138900     END-IF                                                               
139000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
139100     MOVE SPACE TO GODK-STATUSKODER                                       
139200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
139300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139400     PERFORM IMS-STATUSKONTROLL                                           
139500     .                                                                    
139600     EJECT                                                                
139700 IMS-GU-4401 SECTION.                                                     
139800     SKIP2                                                                
139900     STRING 'WLXXDM01(WDGXKEY  =' W-4401-WDGXKEY-X ')'                    
140000            DELIMITED BY SIZE INTO SSA1                                   
140100     MOVE '  ' TO GODK-STATUSKODER                                        
140200     CALL CBLTDLI USING GU XXDM-PCB DLI-IO-AREA SSA1                      
140300     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     SKIP3                                                                
140600     .                                                                    
140700 IMS-GHNP-4402 SECTION.                                                   
140800     SKIP2                                                                
140900     STRING 'WLXXDM11(KY4402   =' W-4402-KY4402-X ')'                     
141000            DELIMITED BY SIZE INTO SSA1                                   
141100     MOVE '  GE' TO GODK-STATUSKODER                                      
141200     CALL CBLTDLI USING GHNP XXDM-PCB DLI-IO-AREA SSA1                    
141300     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
141400     PERFORM IMS-STATUSKONTROLL                                           
141500     .                                                                    
141600     EJECT                                                                
141700 IMS-GNP-4402-KVAL SECTION.                                               
141800     SKIP2                                                                
141900     STRING 'WLXXDM11(KY4402  >=' W-4402-KY4402-MIN-X                     
142000                    '&KY4402  <=' W-4402-KY4402-HI-X                      
142100                    '&IDTRPTNR =' W-4402-IDTRPTNR-X ')'                   
142200            DELIMITED BY SIZE INTO SSA1                                   
142300     MOVE '  GE' TO GODK-STATUSKODER                                      
142400     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-AREA SSA1                     
142500     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     SKIP3                                                                
142800     .                                                                    
142900 IMS-GNP-4402-IDTRPTNR SECTION.                                           
143000     SKIP2                                                                
143100     STRING 'WLXXDM11(IDTRPTNR =' W-4402-IDTRPTNR-X ')'                   
143200            DELIMITED BY SIZE INTO SSA1                                   
143300     MOVE '  GE' TO GODK-STATUSKODER                                      
143400     CALL CBLTDLI USING GNP XXDM-PCB DLI-IO-AREA SSA1                     
143500     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSKONTROLL                                           
143700     .                                                                    
143800     EJECT                                                                
143900 IMS-GU-4405 SECTION.                                                     
144000     SKIP2                                                                
144100     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
144200            DELIMITED BY SIZE INTO SSA1                                   
144300     MOVE '  GE' TO GODK-STATUSKODER                                      
144400     CALL CBLTDLI USING GU XXDN-PCB DLI-IO-AREA SSA1                      
144500     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
144600     PERFORM IMS-STATUSKONTROLL                                           
144700     SKIP3                                                                
144800     .                                                                    
144900 IMS-GNP-4406 SECTION.                                                    
145000     SKIP2                                                                
145100     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
145200            DELIMITED BY SIZE INTO SSA1                                   
145300     MOVE '  GE' TO GODK-STATUSKODER                                      
145400     CALL CBLTDLI USING GNP XXDN-PCB DLI-IO-AREA SSA1                     
145500     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
145600     PERFORM IMS-STATUSKONTROLL                                           
145700     SKIP3                                                                
145800     .                                                                    
145900 IMS-GHNP-4406 SECTION.                                                   
146000     SKIP2                                                                
146100     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
146200            DELIMITED BY SIZE INTO SSA1                                   
146300     MOVE '  GE' TO GODK-STATUSKODER                                      
146400     CALL CBLTDLI USING GHNP XXDN-PCB DLI-IO-AREA SSA1                    
146500     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
146600     PERFORM IMS-STATUSKONTROLL                                           
146700     SKIP3                                                                
146800     .                                                                    
146900 IMS-GHNP-4406-FIRST SECTION.                                             
147000     SKIP2                                                                
147100     STRING 'WLXXDN11*F(WDGXKEY  =' W-4406-WDGXKEY-X ')'                  
147200            DELIMITED BY SIZE INTO SSA1                                   
147300     MOVE '  GE' TO GODK-STATUSKODER                                      
147400     CALL CBLTDLI USING GHNP XXDN-PCB DLI-IO-AREA SSA1                    
147500     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     EJECT                                                                
147900 IMS-GNP-4408 SECTION.                                                    
148000     SKIP2                                                                
148100     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
148200            DELIMITED BY SIZE INTO SSA1                                   
148300     STRING 'WLXXDN21(FLRUTFUL =' W-4408-FLRUTFUL-X ')'                   
148400            DELIMITED BY SIZE INTO SSA2                                   
148500     MOVE '  ' TO GODK-STATUSKODER                                        
148600     CALL CBLTDLI USING GNP XXDN-PCB DLI-IO-AREA SSA1 SSA2                
148700     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     SKIP3                                                                
149000     .                                                                    
149100 IMS-GNP-4408-OKVAL SECTION.                                              
149200     SKIP2                                                                
149300     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
149400            DELIMITED BY SIZE INTO SSA1                                   
149500     MOVE 'WLXXDN21 ' TO SSA2                                             
149600     MOVE '  GE' TO GODK-STATUSKODER                                      
149700     CALL CBLTDLI USING GNP XXDN-PCB DLI-IO-AREA SSA1 SSA2                
149800     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     EJECT                                                                
150200 IMS-GU-4411 SECTION.                                                     
150300     SKIP2                                                                
150400     STRING 'WLXXDO01(WDGXKEY  =' W-4411-WDGXKEY-X ')'                    
150500            DELIMITED BY SIZE INTO SSA1                                   
150600     MOVE '  GE' TO GODK-STATUSKODER                                      
150700     CALL CBLTDLI USING GU XXDO-PCB DLI-IO-AREA SSA1                      
150800     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     SKIP3                                                                
151100     .                                                                    
151200 IMS-GNP-4412 SECTION.                                                    
151300     SKIP2                                                                
151400     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
151500            DELIMITED BY SIZE INTO SSA1                                   
151600     MOVE '  GE' TO GODK-STATUSKODER                                      
151700     CALL CBLTDLI USING GNP XXDO-PCB DLI-IO-AREA SSA1                     
151800     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     SKIP3                                                                
152100     .                                                                    
152200 IMS-GHNP-4414 SECTION.                                                   
152300     SKIP2                                                                
152400     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
152500            DELIMITED BY SIZE INTO SSA1                                   
152600     STRING 'WLXXDO21(TESPAERR =' W-4414-TESPAERR-X ')'                   
152700            DELIMITED BY SIZE INTO SSA2                                   
152800     MOVE '  GE' TO GODK-STATUSKODER                                      
152900     CALL CBLTDLI USING GHNP XXDO-PCB DLI-IO-AREA SSA1 SSA2               
153000     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     EJECT                                                                
153400 IMS-GHNP-4414-KVAL SECTION.                                              
153500     SKIP2                                                                
153600     STRING 'WLXXDO11(WDGXKEY  =' W-4412-WDGXKEY-X ')'                    
153700            DELIMITED BY SIZE INTO SSA1                                   
153800     STRING 'WLXXDO21(WDGXKEY  =' W-4414-WDGXKEY-X ')'                    
153900            DELIMITED BY SIZE INTO SSA2                                   
154000     MOVE '  GE' TO GODK-STATUSKODER                                      
154100     CALL CBLTDLI USING GHNP XXDO-PCB DLI-IO-AREA SSA1 SSA2               
154200     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
154300     PERFORM IMS-STATUSKONTROLL                                           
154400     .                                                                    
154500     EJECT                                                                
154600 IMS-ISRT-4402 SECTION.                                                   
154700     SKIP2                                                                
154800     STRING 'WLXXDM01(WDGXKEY  =' W-4401-WDGXKEY-X ')'                    
154900            DELIMITED BY SIZE INTO SSA1                                   
155000     MOVE  'WLXXDM11 ' TO SSA2                                            
155100     MOVE '  ' TO GODK-STATUSKODER                                        
155200     CALL CBLTDLI USING ISRT XXDM-PCB DLI-IO-AREA SSA1 SSA2               
155300     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSKONTROLL                                           
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-ISRT-4406 SECTION.                                                   
155800     SKIP2                                                                
155900     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
156000            DELIMITED BY SIZE INTO SSA1                                   
156100     MOVE  'WLXXDN11 ' TO SSA2                                            
156200     MOVE '  ' TO GODK-STATUSKODER                                        
156300     CALL CBLTDLI USING ISRT XXDN-PCB DLI-IO-AREA SSA1 SSA2               
156400     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     SKIP3                                                                
156700     .                                                                    
156800 IMS-ISRT-4408 SECTION.                                                   
156900     SKIP2                                                                
157000     STRING 'WLXXDN01(WDGXKEY  =' W-4405-WDGXKEY-X ')'                    
157100            DELIMITED BY SIZE INTO SSA1                                   
157200     STRING 'WLXXDN11(WDGXKEY  =' W-4406-WDGXKEY-X ')'                    
157300            DELIMITED BY SIZE INTO SSA2                                   
157400     MOVE  'WLXXDN21 ' TO SSA3                                            
157500     MOVE '  ' TO GODK-STATUSKODER                                        
157600     CALL CBLTDLI USING ISRT XXDN-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
157700     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     EJECT                                                                
158100 IMS-DLET-XXDM SECTION.                                                   
158200     SKIP2                                                                
158300     MOVE '  ' TO GODK-STATUSKODER                                        
158400     CALL CBLTDLI USING DLET XXDM-PCB DLI-IO-AREA                         
158500     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
158600     PERFORM IMS-STATUSKONTROLL                                           
158700     SKIP3                                                                
158800     .                                                                    
158900 IMS-REPL-XXDM SECTION.                                                   
159000     SKIP2                                                                
159100     MOVE '  ' TO GODK-STATUSKODER                                        
159200     CALL CBLTDLI USING REPL XXDM-PCB DLI-IO-AREA                         
159300     MOVE XXDM-STATUS-CODE TO STATUS-WS                                   
159400     PERFORM IMS-STATUSKONTROLL                                           
159500     SKIP3                                                                
159600     .                                                                    
159700 IMS-DLET-XXDN SECTION.                                                   
159800     SKIP2                                                                
159900     MOVE '  ' TO GODK-STATUSKODER                                        
160000     CALL CBLTDLI USING DLET XXDN-PCB DLI-IO-AREA                         
160100     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
160200     PERFORM IMS-STATUSKONTROLL                                           
160300     SKIP3                                                                
160400     .                                                                    
160500 IMS-REPL-XXDN SECTION.                                                   
160600     SKIP2                                                                
160700     MOVE '  ' TO GODK-STATUSKODER                                        
160800     CALL CBLTDLI USING REPL XXDN-PCB DLI-IO-AREA                         
160900     MOVE XXDN-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200 IMS-REPL-XXDO SECTION.                                                   
161300     SKIP2                                                                
161400     MOVE '  ' TO GODK-STATUSKODER                                        
161500     CALL CBLTDLI USING REPL XXDO-PCB DLI-IO-AREA                         
161600     MOVE XXDO-STATUS-CODE TO STATUS-WS                                   
161700     PERFORM IMS-STATUSKONTROLL                                           
161800     .                                                                    
161900     EJECT                                                                
162000 IMS-STATUSKONTROLL SECTION.                                              
162100     SKIP2                                                                
162200     SET STATUS-IX TO 1                                                   
162300     SEARCH GODK-STATUS AT END CALL FELLOG                                
162400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
162500     END-SEARCH                                                           
162600     CONTINUE                                                             
162700     .                                                                    
