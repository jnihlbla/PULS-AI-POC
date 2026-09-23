000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4031700.                                                
000400 AUTHOR.         CAP GEMINI AB/EP.                                        
000500 DATE-WRITTEN.   DEC   85.                                                
000510 DATE-COMPILED.                                                           
000600                                                                          
000700* PROGRAMMET BEHÖVER KOMPILERAS VID INST. PGA ÄT I DDGS.                  
000800*    FUNKTION.                                                            
000900*        PROGRAMMET UPPDATERAR URSPRUNG I ORDERRADEN FÖR                  
001000*        VISSA DISTRIKT. ENDAST AVVIKELSE MOT PACKUNDERLAG                
001100*        SKALL RAPPORTERAS.                                               
001200*                                                                         
001300*    NYCKELALTERNATIV.                                                    
001400*        PACKARE + ORDERID.                                               
001500*                  ORDERID: - DISTRIKT + KUNDNR + ORDERNR                 
001600*                           - PRODUKTIONSNUMMER                           
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T317                                              
002000*        MID:         W4I31701                                            
002100*                     W4I31301                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O31701                                            
002500*                     W4O31401                                            
002600*                     W4O31501                                            
002700*    EJECT                                                                
002800******************************************************************        
002900*    ÄNDRINGSJOURNAL:                                                     
003000*    901004 - ÄNDRING GJORD AV BOO HAMMARIN, CGLI                         
003100*           - ANPASSNING TILL NYTT UTSEENDE PÅ FYSISK DB WDE4             
003200*           - ANPASSNING TILL NYTT UTSEENDE PÅ FYSISK DB WDE4             
003300*           - BORTTAG AV FYSISK DB RDE5                                   
003400******************************************************************        
003500                                                                          
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP3                                                                
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77    IDPGM                     PIC X(8)    VALUE 'W4031700'.            
004500 77    JA                        PIC X       VALUE 'J'.                   
004510 77    YES                       PIC X       VALUE 'Y'.                   
004600 77    NEJ                       PIC X       VALUE 'N'.                   
004700 77    RAETT                     PIC X       VALUE 'R'.                   
004800 77    FEL                       PIC X       VALUE 'F'.                   
004900 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
005000 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77    ANT-BILD-RADER            PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +267 COMP SYNC.        
005500 77    MIN-MOD-LAENGD            PIC S9(4)   VALUE +100 COMP SYNC.        
005600 77    0605-LAENGD               PIC S9(4)   VALUE +58  COMP SYNC.        
005700 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
005800 77    WS-IDDC                   PIC X(2).                                
005900 77    WS-IDDC-NUM               PIC 9(2).                                
006000 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
006100 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
006200 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
006300 77    WS-IDORDNR                PIC X(5)   VALUE SPACE.                  
006400 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
006500 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
006600 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
006700 77    WS-IDRADNR                PIC 9(4)   VALUE ZERO.                   
006800 77    WS-KDARTURS               PIC X(2)   VALUE SPACE.                  
006900 77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5)  VALUE ZERO COMP-3.            
007000 77    MAX-RAD-ANTAL             PIC S9(3)  VALUE +14  COMP-3.            
007100 77    MAX-RAD-ANTAL-PLUS-1      PIC S9(3)  VALUE +13  COMP-3.            
007200 77    WS-TRAEFF-PACKARE         PIC X(01).                               
007300 77    WS-TRAEFF-RAD             PIC X(01).                               
007400     SKIP2                                                                
007500 77    WS-SLINGA-KLAR            PIC X(01).                               
007600   88  SLINGA-KLAR                          VALUE 'J'.                    
007700 77    VISA-UPPDATERAT-MED-SW    PIC X(01).                               
007800   88  VISA-UPPDATERAT-MED                  VALUE 'J'.                    
007900 77    WS-IDTRANS                PIC X(04).                               
008000   88  WS-SAMMA-BILD                        VALUE '4317'.                 
008100   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
008200                                                  '4313' '4314'           
008300                                                  '4315' '4316'           
008400                                                  '4317' '4318'.          
008500   88  WS-ORDERVIS-TRANS                    VALUE '0605'.                 
008600     SKIP2                                                                
008700 77    WS-INDATA-TEST            PIC X(01).                               
008800   88  WS-INDATA-FEL                        VALUE 'F'.                    
008900   88  WS-INDATA-RATT                       VALUE 'R'.                    
009000     SKIP2                                                                
009100 01    DYNAMISKA-SUBPGM.                                                  
009200    03 CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
009300    03 FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
009400    03 W400ARTU                  PIC X(8)   VALUE 'W400ARTU'.             
009500    03 W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
009600                                                                          
009700 01    FILLER                    PIC X(16) VALUE 'W400ARTU-AREA'.         
009800*01    FILLER  -COPY W400ARTU                                             
009900     SKIP2                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT '.           
010500*01 -COPY WMSGINIT                                                        
010800                                                                          
010900     EJECT                                                                
011000 01    WS-JFR-IDANSTNR.                                                   
011100   03  FILLER                    PIC X(3).                                
011200   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
011300     SKIP2                                                                
011400*01    NYCKLAR-TILL-DLI.                                                  
011500*                                                                         
011510 01  NYCKLAR-TILL-DLI.                                                    
011520   03    W-WDE601KY-X.                                                    
011530     05    W-IDPRODNR-WDE6       PIC S9(7)   VALUE ZERO  COMP-3.          
011560                                                                          
011561   03    W-WDE4F1KY-MAX-X.                                                
011562     05    W-IDPRODNR-WDE4F-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
011563     05    W-IDKOLLI-WDE4F-MAX   PIC S9(5)   VALUE ZERO  COMP-3.          
011564     05    W-WDE4F1-MAX          PIC X(22)   VALUE HIGH-VALUE.            
011565                                                                          
011570   03    W-WDE4F1KY-MIN-X.                                                
011580     05    W-IDPRODNR-WDE4F-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
011590     05    W-IDKOLLI-WDE4F-MIN   PIC S9(5)   VALUE ZERO  COMP-3.          
011591     05    W-WDE4F1-MIN          PIC X(22)   VALUE LOW-VALUE.             
011592                                                                          
011691   03    W-WDE4A1-KUNDORDER-X.                                            
011700     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
011800     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
011900     05    W-4A1-IDKUNDRF.                                                
012000       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
012100       07  FILLER                PIC X(05)   VALUE SPACE.                 
012200*                                                                         
012300   03    W-WDE401-KUNDORDER-X.                                            
012400     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
012500     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
012600     05    W-401-IDKUNDRF.                                                
012700       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
012800       07  FILLER                PIC X(05)   VALUE SPACE.                 
012900     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
013000     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
013100*                                                                         
013200   03    W-WDE4B-KEYSEQ-X.                                                
013300     05    W-420-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
013400     05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
013500*                                                                         
013600   03    W-WDE420-KEYSEQ-X.                                               
013700     05    W-420-IDPURAD2        PIC S9(5)   VALUE ZERO  COMP-3.          
013800*                                                                         
013900     SKIP2                                                                
014000 01  W-RAETT-1.                                                           
014100     03 RAETT-1-SVE              PIC X(21)                                
014200        VALUE 'URSPRUNG UPPDATERAT  '.                                    
014300     03 RAETT-1-ENG              PIC X(21)                                
014400        VALUE 'ORIGIN UPDATED       '.                                    
014500 01  FILLER REDEFINES W-RAETT-1.                                          
014600     03 RAETT-1  OCCURS 2        PIC X(21).                               
014700     SKIP3                                                                
014800 01    MEDDELANDE.                                                        
014900*                                                                         
015000   03    FEL-1.                                                           
015100     05  FILLER                  PIC X(40)   VALUE                        
015200        '701 ORDERN SAKNAS                       '.                       
015300     05  FILLER                  PIC X(40)   VALUE                        
015400        '701 ORDER MISSING                       '.                       
015500   03    FILLER  REDEFINES  FEL-1.                                        
015600     05  FEL-701     OCCURS 2    PIC X(40).                               
015700*                                                                         
015800   03    FEL-2.                                                           
015900     05  FILLER                  PIC X(40)   VALUE                        
016000        '743 MATA IN RADNUMMER OCH URSPRUNG     '.                        
016100     05 FILLER                   PIC X(40)  VALUE                         
016200        '743 ENTER LINE NUMBER AND ORIGIN       '.                        
016300   03    FILLER  REDEFINES  FEL-2.                                        
016400     05  FEL-743     OCCURS 2    PIC X(40).                               
016500*                                                                         
016600   03    FEL-3.                                                           
016700     05  FILLER                  PIC X(40)   VALUE                        
016800        '719 ANGIVEN PACKARE SAKNAS PÅ ORDERN    '.                       
016900     05  FILLER                  PIC X(40)   VALUE                        
017000        '719 WRONG PACKER                        '.                       
017100   03    FILLER  REDEFINES  FEL-3.                                        
017200     05  FEL-719     OCCURS 2    PIC X(40).                               
017300*                                                                         
017400   03    FEL-4.                                                           
017500     05  FILLER                  PIC X(40)   VALUE                        
017600        '744 BÅDE RADNR OCH URSPRUNG MÅSTE ANGES '.                       
017700     05  FILLER                  PIC X(40)   VALUE                        
017800        '744 BOTH LINE NO. AND ORIGIN REQUIRED   '.                       
017900   03    FILLER  REDEFINES  FEL-4.                                        
018000     05  FEL-744     OCCURS 2    PIC X(40).                               
018100*                                                                         
018200   03    FEL-5.                                                           
018300     05  FILLER                  PIC X(40)   VALUE                        
018400        '745 UPPLYSTA RADER TILLHÖR EJ PACKAREN  '.                       
018500     05  FILLER                  PIC X(40)   VALUE                        
018600        '745 HIGHLIT LINES DO NOT BEL. TO PACKER '.                       
018700   03    FILLER  REDEFINES  FEL-5.                                        
018800     05  FEL-745     OCCURS 2    PIC X(40).                               
018900*                                                                         
019000   03    FEL-6.                                                           
019100     05  FILLER                  PIC X(40)   VALUE                        
019200        '748 UPPLYSTA FÄLT FEL                   '.                       
019300     05  FILLER                  PIC X(40)   VALUE                        
019400        '748 HIGH-LIGHTED  FIELDS WRONG          '.                       
019500   03    FILLER  REDEFINES  FEL-6.                                        
019600     05  FEL-748     OCCURS 2    PIC X(40).                               
019700*                                                                         
019800   03    FEL-7.                                                           
019900     05  FILLER                  PIC X(40)   VALUE                        
020000        '804 AVVIKELSEKONTROLL PÅGÅR             '.                       
020100     05  FILLER                  PIC X(40)   VALUE                        
020200        '804 DEVIATION CONTROL IN PROGRESS       '.                       
020300   03    FILLER  REDEFINES  FEL-7.                                        
020400     05  FEL-804     OCCURS 2    PIC X(40).                               
020410*                                                                         
020420   03    FEL-8.                                                           
020430     05  FILLER                  PIC X(40)   VALUE                        
020440        '749 ANGE ISO KOD FÖR LAND               '.                       
020450     05  FILLER                  PIC X(40)   VALUE                        
020460        '749 WRITE ISO CODE FOR COUNTRY          '.                       
020470   03    FILLER  REDEFINES  FEL-8.                                        
020480     05  FEL-749     OCCURS 2    PIC X(40).                               
020500*                                                                         
020510   03    FEL-9.                                                           
020520     05  FILLER                  PIC X(40)   VALUE                        
020530        '750 RADNR INTE NUMERISKT                '.                       
020540     05  FILLER                  PIC X(40)   VALUE                        
020550        '750 LINE NO IS NOT NUMERIC              '.                       
020560   03    FILLER  REDEFINES  FEL-9.                                        
020570     05  FEL-750     OCCURS 2    PIC X(40).                               
020580*                                                                         
020590   03    FEL-10.                                                          
020591     05  FILLER                  PIC X(40)   VALUE                        
020592        '751 ISO KOD SAKNAS PÅ REGISTER          '.                       
020593     05  FILLER                  PIC X(40)   VALUE                        
020594        '751 ISO CODE MISSING IN REGISTER        '.                       
020595   03    FILLER  REDEFINES  FEL-10.                                       
020596     05  FEL-751     OCCURS 2    PIC X(40).                               
020597*                                                                         
020600     SKIP2                                                                
020700******************************************************************        
020800*                                                                *        
020900*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
021000*                                                                *        
021100******************************************************************        
021200 01    FILLER                 PIC X(16) VALUE 'MID W4I31701 MID'.         
021300*01    MID -COPY W4I31701.                                                
021400*                                                                         
021800     EJECT                                                                
021900 01    FILLER                 PIC X(08) VALUE 'WMSGAREA'.                 
022000*01    -COPY WMSGAREA                                                     
022100*  03    MOD -COPY W4O31701.                                              
022200     EJECT                                                                
022300*  03    MID -COPY W4I31801  -PRE 4318-.                                  
022400     EJECT                                                                
022500*  03    MID -COPY W0I60502  -PRE 0605-.                                  
022600     EJECT                                                                
022630 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
022640 01  P-TO-P-SW1.                                                          
022650     03  PTOP1-LL                PIC S9(4)   VALUE 00 COMP SYNC.          
022660     03  PTOP1-Z1                PIC  X(1)   VALUE LOW-VALUE.             
022670     03  PTOP1-Z2                PIC  X(1)   VALUE LOW-VALUE.             
022680     03  PTOP1-TRANSKOD          PIC  X(7)   VALUE 'W4T313 '.             
022690     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
022691     03  FILLER                  PIC  X(4)   VALUE '4317'.                
022692     03  PTOP1-KDMFSFOR          PIC  X(1).                               
022693     03  -COPY W4I31301  -PRE PTOP1-                                      
022694     EJECT                                                                
022700 01    FILLER                 PIC X(16) VALUE 'MOD W4O31401 MOD'.         
022800*01      MOD -COPY W4O31401  -PRE 4314-.                                  
022900     EJECT                                                                
023000 01    FILLER                 PIC X(16) VALUE 'MOD W4O31501 MOD'.         
023100*01      MOD -COPY W4O31501  -PRE 4315-.                                  
023200     EJECT                                                                
023300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
023400     SKIP3                                                                
023500*01    -COPY WMFSAREA                                                     
023600     EJECT                                                                
023700******************************************************************        
023800 01  FILLER                      PIC X(16) VALUE 'USER-SPAR-AREA'.        
023900 01  SPAR-AREA.                                                           
024000   03  SPAR-IDTRANS              PIC X(4)    VALUE '4313'.                
024100   03  SPAR-W4I31301.                                                     
024200     05  -COPY W4I31301                                                   
024300******************************************************************        
024400*                                                                         
024500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024600*                                                                         
024700 01    IMS-WS.                                                            
024800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
024900     SKIP3                                                                
025000*                        **** STATUS-KOD FRÅN IMS                         
025100   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
025200     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
025300     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
025400   03    STATUS-WS               PIC XX.                                  
025500     88    SEGMENT-FINNS                     VALUE '  '.                  
025600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
025700     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
025800     SKIP3                                                                
025900   03    GODK-STATUSKODER.                                                
026000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
026100     SKIP3                                                                
026200 01    SSA1                      PIC X(64).                               
026300 01    SSA2                      PIC X(64).                               
026400 01    SSA3                      PIC X(64).                               
026500 01    SSA4                      PIC X(64).                               
026600     EJECT                                                                
026700*                            IMS FUNKTIONSKODER                           
026800*01    -COPY W0003                                                        
026900     EJECT                                                                
027000*                            DLI INPUT-OUTPUT AREA                        
027100 01    DLI-IO-AREA.                                                       
027200   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
027300     SKIP3                                                                
027400*  03    WDE401   -COPY WDE401             -RED IO-AREA.                  
027500     EJECT                                                                
027600*  03    WDE411   -COPY WDE411             -RED IO-AREA.                  
027700     EJECT                                                                
027800*  03    WDE601   -COPY WDE601             -RED IO-AREA.                  
027900     EJECT                                                                
027910 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE4F1'.               
027920 01  DLI-IO-WDE4F1.                                                       
027930*    03  -COPY WDE4F1  -PRE WDE4F-                                        
027940 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE401'.               
027950 01  DLI-IO-WDE401.                                                       
027960*    03  -COPY WDE401  -PRE WDE4-                                         
027970 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDE601'.               
027980 01  DLI-IO-WDE601.                                                       
027990*    03  -COPY WDE601  -PRE WDE6-                                         
027991     EJECT                                                                
028000 LINKAGE SECTION.                                                         
028100*01    -COPY W0009     -PRE MSG-                                          
028200     EJECT                                                                
028300*01    -COPY W0009     -PRE ALT0605-                                      
028400     EJECT                                                                
028500*01    -COPY W0009     -PRE ALT4313-                                      
028600     EJECT                                                                
028700*01    -COPY W0009     -PRE ALT-                                          
028800     EJECT                                                                
028900*01    -COPY W0008     -PRE USEA-                                         
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01    -COPY W0008     -PRE WDE4-                                         
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01    -COPY W0008     -PRE WDE4A-                                        
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01    -COPY W0008     -PRE WDE4B-                                        
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030210*01    -COPY W0008     -PRE WDE6-                                         
030220     05  FILLER                  PIC X.                                   
030221     EJECT                                                                
030400 PROCEDURE DIVISION USING  MSG-PCB ALT0605-PCB ALT4313-PCB ALT-PCB        
030500                           USEA-PCB WDE4-PCB WDE4A-PCB                    
030600                           WDE4B-PCB WDE6-PCB.                            
030710 MAIN SECTION.                                                            
030800     ENTRY 'DLITCBL' USING MSG-PCB ALT0605-PCB ALT4313-PCB                
030900                           ALT-PCB                                        
031000                           USEA-PCB WDE4-PCB WDE4A-PCB                    
031110                           WDE4B-PCB WDE6-PCB.                            
031307                                                                          
031310     PERFORM IMS-GET-MSG                                                  
031400     IF SEGMENT-FINNS                                                     
031500         PERFORM A-INIT                                                   
031600*                                                                         
031700         EVALUATE TRUE                                                    
031800         WHEN WS-SAMMA-BILD                                               
031900         OR   WS-ORDERVIS-TRANS                                           
032000             PERFORM B-GENERELL-KONTROLL                                  
032100*                                                                         
032200             IF WS-INDATA-RATT                                            
032300                 PERFORM C-RELATIONSKONTROLL                              
032400*                                                                         
032500                 IF WS-INDATA-RATT                                        
032600*                                                                         
032700                     MOVE +1 TO INX                                       
032800                     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL                
032900                         PERFORM D-BEHANDLA-RAD                           
033000                         ADD +1 TO INX                                    
033100                     END-PERFORM                                          
033200                 END-IF                                                   
033300             END-IF                                                       
033400             MOVE MAX-MOD-LAENGD TO  MSG-KVLL                             
033500         WHEN WS-GODKAND-BILD                                             
033600             MOVE '4317'         TO  MFS-IDTRANS                          
033700             MOVE +96            TO  MSG-KVLL                             
033800             MOVE FEL            TO  WS-INDATA-TEST                       
033900             MOVE 'W4O31701'     TO  MFS-IDMOD                            
034000         WHEN OTHER                                                       
034100             MOVE '4317'         TO  MFS-IDTRANS                          
034200             MOVE +8             TO  MSG-KVLL                             
034300             MOVE FEL            TO  WS-INDATA-TEST                       
034400             MOVE 'W4O31701'     TO  MFS-IDMOD                            
034500         END-EVALUATE                                                     
034600         IF WS-INDATA-RATT                                                
034700             PERFORM H-AVSLUT                                             
034800         ELSE                                                             
034900             IF  WS-ORDERVIS-TRANS                                        
035000                 MOVE '0605'       TO  MFS-IDTRANS                        
035100                 MOVE 'W0T605U '   TO  MSG-KDTRANS-1                      
035200                 MOVE '4317'       TO  MSG-IDTRANS-1                      
035300                 MOVE WS-KDMFSFOR  TO  MSG-KDMFSFOR-1                     
035400                 MOVE 0605-LAENGD  TO  MSG-KVLL                           
035500                 MOVE NEJ          TO  0605-MID-FLSVAR                    
035600                 MOVE MOD-TEMFSFEL TO 0605-MID-TEMFSFEL                   
035700                 MOVE 0605-MID                                            
035800                             TO MSG-INDATA-MINUS-1-TRANSKOD               
035900             ELSE                                                         
036000                 MOVE '4317'       TO  MFS-IDTRANS                        
036100                 MOVE MOD-W4O31701 TO  MSG-AREA                           
036200             END-IF                                                       
036300         END-IF                                                           
036400         EVALUATE TRUE                                                    
036500         WHEN MFS-IDTRANS = '4313'                                        
036600             PERFORM IMS-INSERT-ALT4313-MSG                               
036700         WHEN MFS-IDTRANS = '4317'                                        
036800             PERFORM IMS-INSERT-MSG                                       
036900         WHEN MFS-IDTRANS = '0605'                                        
037000             PERFORM IMS-INSERT-ALT0605MSG                                
037112         WHEN OTHER                                                       
037200             PERFORM IMS-CHANGE-ALTMSG                                    
037300             PERFORM IMS-INSERT-ALTMSG                                    
037400         END-EVALUATE                                                     
037500     END-IF                                                               
037600     MOVE ZERO TO RETURN-CODE                                             
037700     GOBACK                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 A-INIT             SECTION.                                              
038100                                                                          
038200     IF MSG-DUBBLA-TRANSKODER                                             
038300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I31701               
038400       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
038500                                               WS-IDTRANS                 
038600       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
038700       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
038800     ELSE                                                                 
038900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I31701               
039000       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
039100                                               WS-IDTRANS                 
039200       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
039300       MOVE ' '                           TO   MFS-KDTRTYP                
039400     END-IF                                                               
039500*                                                                         
039600     MOVE LOW-VALUE                       TO   MSG-AREA                   
039700     MOVE 'W4O317N1'                      TO   MFS-IDMOD                  
039800     MOVE '4317'                          TO   MOD-IDTRANS                
039900     MOVE MID-IDTRANS-START               TO   MOD-IDTRANS-START          
040000     MOVE MID-FLSISTAK                    TO   MOD-FLSISTAK               
040100*                                                                         
040200     MOVE RAETT                           TO WS-INDATA-TEST               
040300     MOVE NEJ                           TO VISA-UPPDATERAT-MED-SW         
040400     PERFORM AA-FLYTTA-NYCKLAR                                            
040500     PERFORM AB-HAMTA-DATA-FR-USER-DB                                     
040600     MOVE MFS-RENSA-FAELT                 TO   MOD-TEMFSFEL               
040700                                               MOD-IDANSTNR-IN            
040800                                               MOD-IDDISTR-IN             
040900                                               MOD-IDKUNDNR-IN            
041000                                               MOD-IDORDNR-IN             
041100                                               MOD-IDKOLLI-IN             
041200                                               MOD-IDPRODNR-IN            
041300                                               MOD-TEMFSINF               
041400     SKIP2                                                                
041500     MOVE +1  TO RAD-INX                                                  
041600     MOVE +14 TO MAX-RAD-ANTAL                                            
041700     PERFORM UNTIL RAD-INX NOT < MAX-RAD-ANTAL                            
041800         MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR (RAD-INX)              
041900                                       MOD-KDARTURS (RAD-INX)             
042000         ADD +1 TO RAD-INX                                                
042100     END-PERFORM                                                          
042200     .                                                                    
042300     EJECT                                                                
042400 AA-FLYTTA-NYCKLAR  SECTION.                                              
042500                                                                          
042600     IF MID-IDANSTNR-IN = ALL '+'                                         
042700         MOVE MID-IDANSTNR-UT             TO   WS-IDANSTNR                
042800         INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO              
042900     ELSE                                                                 
043000         MOVE MID-IDANSTNR-IN             TO   WS-IDANSTNR                
043100     END-IF                                                               
043200                                                                          
043300     IF MID-IDPRODNR-IN = ALL '+'                                         
043400         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
043500         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
043600     ELSE                                                                 
043700         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
043800     END-IF                                                               
043900                                                                          
044000     IF MID-IDDISTR-IN = ALL '+'                                          
044100         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
044200         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
044300     ELSE                                                                 
044400         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
044500     END-IF                                                               
044600                                                                          
044700     IF MID-IDKUNDNR-IN = ALL '+'                                         
044800         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
044900         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
045000     ELSE                                                                 
045100         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
045200     END-IF                                                               
045300                                                                          
045400     IF MID-IDORDNR-IN = ALL '+'                                          
045500         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
045600         INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO               
045700     ELSE                                                                 
045800         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
045900     END-IF                                                               
046000                                                                          
046100     IF MID-IDKOLLI-IN = ALL '+'                                          
046200         MOVE MID-IDKOLLI-UT              TO   WS-IDKOLLI                 
046300         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
046400     ELSE                                                                 
046500         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
046600     END-IF                                                               
046700                                                                          
046800     MOVE WS-IDANSTNR                     TO   MOD-IDANSTNR-UT            
046900     INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE              
047000     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
047100     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
047200     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
047300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
047400     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
047500     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
047600     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
047700     INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE               
047800     MOVE WS-IDPRODNR                     TO   MOD-IDPRODNR-UT            
047900     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
048000     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
048100     .                                                                    
048200     EJECT                                                                
048300 AB-HAMTA-DATA-FR-USER-DB     SECTION.                                    
048400                                                                          
048500     MOVE ALL '+'               TO MSGI-WMSGINIT                          
048600     MOVE '001'                 TO MSGI-KDCALL                            
048700     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
048800     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
048900     MOVE '4317'                TO MSGI-IDTRANS                           
049000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
049100                                                                          
049200     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
049300                                                                          
049400     MOVE MSGI-IDDC             TO WS-IDDC                                
049500                                                                          
049600     IF MSGI-IDLAND-SPR = 'GB'                                            
049700       MOVE +2 TO INDX                                                    
049800                  WS-KDMFSFOR                                             
049900     ELSE                                                                 
050000       MOVE +1 TO INDX                                                    
050100                  WS-KDMFSFOR                                             
050200     END-IF                                                               
050300                                                                          
050400     IF WS-IDDC IS > SPACE                                                
050500       MOVE WS-IDDC             TO MOD-IDDC-UT                            
050600     ELSE                                                                 
050700       MOVE NEJ                 TO WS-INDATA-TEST                         
050800     END-IF                                                               
050900     .                                                                    
051000     SKIP2                                                                
051100 B-GENERELL-KONTROLL  SECTION.                                            
051200                                                                          
051300     IF WS-IDANSTNR NOT NUMERIC                                           
051400         MOVE FEL                       TO   WS-INDATA-TEST               
051500         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
051600     END-IF                                                               
051700                                                                          
051800     IF WS-IDDISTR  NOT NUMERIC                                           
051900         MOVE FEL                       TO   WS-INDATA-TEST               
052000         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
052100     ELSE                                                                 
052200         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
052300     END-IF                                                               
052400                                                                          
052500     IF WS-IDKUNDNR NOT NUMERIC                                           
052600         MOVE FEL                       TO   WS-INDATA-TEST               
052700         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
052800     ELSE                                                                 
052900         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
053000     END-IF                                                               
053100                                                                          
053200     IF WS-IDORDNR  NOT NUMERIC                                           
053300         MOVE FEL                       TO   WS-INDATA-TEST               
053400         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
053500     ELSE                                                                 
053600         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
053700     END-IF                                                               
053800                                                                          
053900     IF WS-IDKOLLI NOT NUMERIC                                            
054000         MOVE FEL                       TO   WS-INDATA-TEST               
054100         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
054200     END-IF                                                               
054300                                                                          
054400     IF WS-IDPRODNR NOT NUMERIC                                           
054500         MOVE FEL                       TO   WS-INDATA-TEST               
054600         MOVE FEL-748 (INDX)            TO   MOD-TEMFSFEL                 
054700     END-IF                                                               
054800                                                                          
054900     IF WS-IDDC IS <= SPACE                                               
055000       MOVE FEL                         TO WS-INDATA-TEST                 
055100       MOVE FEL-748 (INDX)              TO MOD-TEMFSFEL                   
055200     END-IF                                                               
055300*                                                                         
055400     MOVE ZERO TO ANT-BILD-RADER                                          
055500     MOVE +1   TO INX                                                     
055600     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL                                
055700         MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR (INX)                      
055800                                   MOD-KDARTURS (INX)                     
055900         PERFORM BA-KONTROLLERA-RAD                                       
056000         ADD +1 TO INX                                                    
056100     END-PERFORM                                                          
056200     SKIP2                                                                
056300     IF MID-FLSISTAK    = ALL '+'                                         
056400         MOVE SPACE                       TO  MOD-FLSISTAK                
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800 BA-KONTROLLERA-RAD     SECTION.                                          
056900     SKIP3                                                                
057000     IF MID-IDRADNR (INX) = ALL '+'                                       
057100         MOVE MFS-RENSA-FAELT          TO MOD-IDRADNR (INX)               
057200                                          MOD-KDARTURS (INX)              
057300*------------ OM MAN EJ VILL GODKÄNNA TOM BILD.                           
057400*        IF INX = +1                                                      
057500*            MOVE FEL                  TO WS-INDATA-TEST                  
057600*            MOVE FEL-743 (INDX)       TO MOD-TEMFSFEL                    
057700*            MOVE MFS-NUM-FAELT-FEL    TO MOD-IDRADNR-ATTR (INX)          
057800*        END-IF                                                           
057900         MOVE +14                      TO INX                             
058000     ELSE                                                                 
058100         MOVE INX TO ANT-BILD-RADER                                       
058200         IF MID-KDARTURS (INX) = ALL '+'                                  
058300             MOVE SPACE                TO MOD-KDARTURS (INX)              
058400             MOVE FEL                  TO WS-INDATA-TEST                  
058500             MOVE FEL-744 (INDX)       TO MOD-TEMFSFEL                    
058600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARTURS-ATTR (INX)         
058700         ELSE                                                             
058800             INSPECT MID-IDRADNR (INX) REPLACING LEADING SPACE            
058900                                BY ZERO                                   
059000             IF MID-IDRADNR (INX) NUMERIC                                 
059100                 MOVE MFS-NUM-FAELT-RAETT TO                              
059200                                    MOD-IDRADNR-ATTR (INX)                
059300             ELSE                                                         
059400                 MOVE FEL                 TO WS-INDATA-TEST               
059500                 MOVE FEL-750 (INDX)      TO  MOD-TEMFSFEL                
059600                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR (INX)         
059700             END-IF                                                       
059710                                                                          
059800             INSPECT MID-KDARTURS (INX) REPLACING LEADING ZERO            
059900                                BY SPACE                                  
060000             IF MID-KDARTURS (INX) NUMERIC                                
061700               MOVE FEL                TO WS-INDATA-TEST                  
061800               MOVE FEL-749 (INDX)     TO MOD-TEMFSFEL                    
061900               MOVE MFS-ALFA-FAELT-FEL TO                                 
062000                              MOD-KDARTURS-ATTR (INX)                     
062001             ELSE                                                         
062010               MOVE MID-KDARTURS (INX)     TO ARTU-KDARTURS               
062020               MOVE WS-IDDISTR             TO ARTU-IDDISTR                
062030               MOVE WS-IDDC                TO ARTU-IDDC                   
062040               CALL W400ARTU USING ARTU-W400ARTU                          
062050                                                                          
062060               IF ARTU-KDARTURS-NUM NUMERIC                               
062105                   MOVE MID-KDARTURS (INX) TO MOD-KDARTURS (INX)          
062106                   MOVE MFS-ALFA-FAELT-RAETT TO                           
062107                                    MOD-KDARTURS-ATTR (INX)               
062108               ELSE                                                       
062109                   MOVE FEL                TO WS-INDATA-TEST              
062110                   MOVE FEL-751 (INDX)     TO MOD-TEMFSFEL                
062111                   MOVE MFS-ALFA-FAELT-FEL TO                             
062112                                  MOD-KDARTURS-ATTR (INX)                 
062123               END-IF                                                     
062130             END-IF                                                       
062200         END-IF                                                           
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 C-RELATIONSKONTROLL  SECTION.                                            
062700                                                                          
062800     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
062900     IF  WS-INDATA-RATT                                                   
063000       IF WS-SAMMA-BILD                                                   
063100         PERFORM CB-KONTROLLERA-PACKARE                                   
063200         IF WS-INDATA-RATT                                                
063300             PERFORM CC-BEHANDLA-RADER                                    
063400             COMPUTE MAX-RAD-ANTAL = ANT-BILD-RADER + 1                   
063500         ELSE                                                             
063600             PERFORM S04-ADD-LAES-IN-FAELT                                
063700         END-IF                                                           
063800       ELSE                                                               
063900         COMPUTE MAX-RAD-ANTAL = ANT-BILD-RADER + 1                       
064000       END-IF                                                             
064100     END-IF                                                               
064200     .                                                                    
064300     EJECT                                                                
064400 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
064500                                                                          
064600     IF MID-IDPRODNR-IN = ALL '+'                                         
064700         IF    MID-IDDISTR-IN  = ALL '+'                                  
064800           AND MID-IDKUNDNR-IN = ALL '+'                                  
064900           AND MID-IDORDNR-IN  = ALL '+'                                  
065000             IF WS-IDPRODNR > ZERO                                        
065100*                << ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT >>              
065200                 MOVE JA              TO  SOEK-VIA-PRODNR                 
065300                 MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR-UT                  
065400                                          MOD-IDKUNDNR-UT                 
065500                                          MOD-IDORDNR-UT                  
065600             ELSE                                                         
065700                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
065800                 MOVE MFS-RENSA-FAELT TO  MOD-IDPRODNR-UT                 
065900             END-IF                                                       
066000         ELSE                                                             
066100             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
066200             MOVE MFS-RENSA-FAELT     TO  MOD-IDPRODNR-UT                 
066300         END-IF                                                           
066400     ELSE                                                                 
066500         MOVE JA                       TO SOEK-VIA-PRODNR                 
066600         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-UT                  
066700                                          MOD-IDKUNDNR-UT                 
066800                                          MOD-IDORDNR-UT                  
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
067300                                                                          
067400     MOVE 'N'                  TO WS-SLINGA-KLAR                          
067500*                                                                         
067600     MOVE WS-IDDISTR           TO W-4A1-IDDISTR                           
067700     MOVE WS-IDKUNDNR          TO W-4A1-IDKUNDNR                          
067800     MOVE WS-IDORDNR           TO W-4A1-IDORDNR                           
067900     PERFORM IMS-GU-KUNDORDER-SEK                                         
068000*                                                                         
068100     IF KUNDORDER-SEK-FINNS                                               
068200        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
068300                      SLINGA-KLAR                                         
068400        MOVE KORD-IDDISTR      TO W-401-IDDISTR                           
068500        MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                          
068600        MOVE KORD-IDORDNR5     TO W-401-IDORDNR                           
068700        MOVE KORD-IDPRODNR     TO W-401-IDPRODNR                          
068710        MOVE KORD-IDPRODNR     TO W-IDPRODNR-WDE6                         
068800        MOVE KORD-IDPLKLST     TO W-401-IDPLKLST                          
068900        PERFORM IMS-GU-KUNDORDER                                          
069000        MOVE KORD-KVORDRAD-LEVPL   TO WS-SPAR-KVORDRAD-LEVPL              
069100*                                                                         
069200        PERFORM IMS-GU-IDPRODNR                                           
071000        IF SEGMENT-FINNS                                                  
071100           IF WDE6-VORD-IDDC = WS-IDDC                                    
071200              MOVE 'J'                 TO   WS-SLINGA-KLAR                
071300              MOVE WDE6-VORD-IDPRODNR       TO   WS-IDPRODNR              
071400           ELSE                                                           
071500              MOVE FEL                 TO   WS-INDATA-TEST                
071600              MOVE FEL-701 (INDX)      TO   MOD-TEMFSFEL                  
071700           END-IF                                                         
071800        END-IF                                                            
071900        PERFORM IMS-GN-KUNDORDER-SEK                                      
072000        END-PERFORM                                                       
072100     END-IF                                                               
072200*                                                                         
072300     IF NOT SLINGA-KLAR                                                   
072400        MOVE FEL                         TO   WS-INDATA-TEST              
072500        MOVE FEL-701 (INDX)              TO   MOD-TEMFSFEL                
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 CB-KONTROLLERA-PACKARE SECTION.                                          
073000                                                                          
073100     MOVE 'N'                           TO WS-TRAEFF-PACKARE              
073200*                                                                         
073300     MOVE WS-IDDISTR                    TO W-4A1-IDDISTR                  
073400     MOVE WS-IDKUNDNR                   TO W-4A1-IDKUNDNR                 
073500     MOVE WS-IDORDNR                    TO W-4A1-IDORDNR                  
073600     PERFORM IMS-GU-KUNDORDER-SEK                                         
073700*                                                                         
073800     IF KUNDORDER-SEK-FINNS                                               
073900        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
074000                      WS-TRAEFF-PACKARE = 'J'                             
074100        MOVE KORD-IDDISTR              TO W-401-IDDISTR                   
074200        MOVE KORD-IDKUNDNR             TO W-401-IDKUNDNR                  
074300        MOVE KORD-IDORDNR5             TO W-401-IDORDNR                   
074400        MOVE KORD-IDPRODNR             TO W-401-IDPRODNR                  
074500        MOVE KORD-IDPLKLST             TO W-401-IDPLKLST                  
074600        PERFORM IMS-GU-KUNDORDER                                          
074700*                                                                         
074800        MOVE KORD-IDPRODNR             TO WS-JFR-IDPRODNR                 
074900        MOVE KORD-IDUSER               TO WS-JFR-IDANSTNR                 
075000        IF NOT WS-ORDERVIS-TRANS AND                                      
075100           WS-IDPRODNR = WS-JFR-IDPRODNR AND                              
075200           WS-IDANSTNR = WS-JFR-IDANSTNR-5                                
075300           MOVE 'J'                    TO WS-TRAEFF-PACKARE               
075400           IF KORD-KDPAKOLL NOT = ZERO                                    
075500*----------------------------------------------AVVIKELSEKONTROLL          
075600*----------------------------------------------PÅGÅR                      
075700              MOVE FEL                 TO WS-INDATA-TEST                  
075800              MOVE FEL-804 (INDX)      TO MOD-TEMFSFEL                    
075900           END-IF                                                         
076000        END-IF                                                            
076100*                                                                         
076200        PERFORM IMS-GN-KUNDORDER-SEK                                      
076300        END-PERFORM                                                       
076400     ELSE                                                                 
076500        MOVE FEL                         TO   WS-INDATA-TEST              
076600        MOVE FEL-701 (INDX)              TO   MOD-TEMFSFEL                
076700     END-IF                                                               
076800*                                                                         
076900     IF WS-TRAEFF-PACKARE = 'N' AND                                       
077000        NOT WS-ORDERVIS-TRANS                                             
077100*----------------------------------------------ANGIVEN PACKARE            
077200*----------------------------------------------SAKNAS PÅ ORDERN           
077300        MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                        
077400                                    MOD-IDKUNDNR-UT                       
077500                                    MOD-IDORDNR-UT                        
077600        MOVE FEL                 TO WS-INDATA-TEST                        
077700        MOVE FEL-719 (INDX)      TO MOD-TEMFSFEL                          
077800     END-IF                                                               
077900     .                                                                    
078000     EJECT                                                                
078100 CC-BEHANDLA-RADER       SECTION.                                         
078200                                                                          
078300     IF ANT-BILD-RADER > ZERO                                             
078400        MOVE +1 TO INX                                                    
078500        PERFORM UNTIL INX > ANT-BILD-RADER                                
078600*                                                                         
078700        MOVE MID-IDRADNR (INX) TO WS-IDRADNR                              
078800        MOVE 'N'               TO WS-TRAEFF-RAD                           
078900*                                                                         
079000        MOVE WS-IDDISTR        TO W-4A1-IDDISTR                           
079100        MOVE WS-IDKUNDNR       TO W-4A1-IDKUNDNR                          
079200        MOVE WS-IDORDNR        TO W-4A1-IDORDNR                           
079300        PERFORM IMS-GU-KUNDORDER-SEK                                      
079400*                                                                         
079500        IF KUNDORDER-SEK-FINNS                                            
079600           PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                          
079700                         WS-TRAEFF-RAD = 'J'                              
079800           MOVE KORD-IDDISTR   TO W-401-IDDISTR                           
079900           MOVE KORD-IDKUNDNR  TO W-401-IDKUNDNR                          
080000           MOVE KORD-IDORDNR5  TO W-401-IDORDNR                           
080100           MOVE KORD-IDPRODNR  TO W-401-IDPRODNR                          
080200           MOVE KORD-IDPLKLST  TO W-401-IDPLKLST                          
080300           PERFORM IMS-GU-KUNDORDER                                       
080400*                                                                         
080500           MOVE KORD-IDPRODNR  TO WS-JFR-IDPRODNR                         
080600           MOVE KORD-IDUSER    TO WS-JFR-IDANSTNR                         
080700           IF WS-IDPRODNR = WS-JFR-IDPRODNR                               
080800              PERFORM CCA-KONTROLLERA-RADER                               
080900           END-IF                                                         
081000*                                                                         
081100           PERFORM IMS-GN-KUNDORDER-SEK                                   
081200           END-PERFORM                                                    
081300           IF WS-TRAEFF-RAD = 'N'                                         
081400              MOVE FEL               TO WS-INDATA-TEST                    
081500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR (INX)            
081600              MOVE FEL-745 (INDX)    TO MOD-TEMFSFEL                      
081700           END-IF                                                         
081800        END-IF                                                            
081900        ADD +1 TO INX                                                     
082000        END-PERFORM                                                       
082100     END-IF                                                               
082200     .                                                                    
082300     EJECT                                                                
082400 CCA-KONTROLLERA-RADER SECTION.                                           
082500                                                                          
082600     MOVE WS-IDRADNR                 TO W-420-IDPURAD2                    
082700*                                                                         
082800     IF NOT WS-ORDERVIS-TRANS AND                                         
082900        WS-IDANSTNR = WS-JFR-IDANSTNR-5 OR                                
083000        WS-ORDERVIS-TRANS                                                 
083100        PERFORM IMS-GU-RAD                                                
083200*                                                                         
083300        IF SEGMENT-FINNS                                                  
083400           MOVE 'J'               TO WS-TRAEFF-RAD                        
083500        END-IF                                                            
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900 D-BEHANDLA-RAD       SECTION.                                            
084000                                                                          
084100     MOVE WS-IDPRODNR            TO  W-420-IDPRODNR                       
084200     MOVE MID-IDRADNR (INX)      TO  W-420-IDPURAD                        
084300     MOVE MID-KDARTURS (INX)     TO  WS-KDARTURS                          
084400     SKIP2                                                                
084500     PERFORM IMS-GHU-RAD-SEK                                              
084600     SKIP2                                                                
084700     MOVE WS-KDARTURS            TO  ORAD-KDARTURS                        
084800     SKIP2                                                                
084900     PERFORM IMS-REPL-RAD                                                 
085000     IF SEGMENT-FINNS                                                     
085100       MOVE JA                   TO  VISA-UPPDATERAT-MED-SW               
085200     END-IF                                                               
085300     .                                                                    
085400     EJECT                                                                
085500 H-AVSLUT             SECTION.                                            
085600                                                                          
085700     IF  WS-ORDERVIS-TRANS                                                
085800         MOVE '0605'                     TO MFS-IDTRANS                   
085900         MOVE 'W0T605U '                 TO MSG-KDTRANS-1                 
086000         MOVE '4317'                     TO MSG-IDTRANS-1                 
086100         MOVE WS-KDMFSFOR                TO MSG-KDMFSFOR-1                
086200         MOVE 0605-LAENGD                TO MSG-KVLL                      
086300         MOVE JA                         TO 0605-MID-FLSVAR               
086400         MOVE SPACE                      TO 0605-MID-TEMFSFEL             
086500         MOVE 0605-MID                                                    
086600                     TO MSG-INDATA-MINUS-1-TRANSKOD                       
086700     ELSE                                                                 
086800         PERFORM S02-RENSA-MOD-FAELT                                      
086900         EVALUATE TRUE                                                    
087000         WHEN MID-IDRADNR (13) NOT = ALL '+'                              
087100             MOVE MID-IDRADNR (13)       TO MOD-IDRADNR-S                 
087200             MOVE MID-KDARTURS (13)      TO MOD-KDARTURS-S                
087300             MOVE '4317'                 TO MFS-IDTRANS                   
087400             IF VISA-UPPDATERAT-MED                                       
087500               MOVE RAETT-1 (INDX)       TO MOD-TEMFSINF                  
087600             END-IF                                                       
087700                                                                          
087800             IF  MID-IDTRANS-START = '4313' OR '4314' OR '4315'           
087900                 MOVE MID-IDTRANS-START  TO MOD-IDTRANS-START             
088000             END-IF                                                       
088100                                                                          
088200             IF  MID-FLSISTAK = JA OR YES                                 
088300                 MOVE MID-FLSISTAK       TO MOD-FLSISTAK                  
088400             END-IF                                                       
088500                                                                          
088600             MOVE MOD-W4O31701           TO MSG-AREA                      
088700         WHEN MID-FLSISTAK = JA OR YES                                    
088800             MOVE '4318'                 TO MFS-IDTRANS                   
088900             MOVE 'W4T318U '             TO MSG-KDTRANS-1                 
089000             MOVE '4317'                 TO MSG-IDTRANS-1                 
089100             MOVE WS-KDMFSFOR            TO MSG-KDMFSFOR-1                
089200             COMPUTE MSG-KVLL = LENGTH OF 4318-MID-W4I31801 + 17          
089300             PERFORM HC-LADDA-4318                                        
089400             MOVE 4318-MID                                                
089500                     TO MSG-INDATA-MINUS-1-TRANSKOD                       
089600         WHEN OTHER                                                       
089700             IF VISA-UPPDATERAT-MED                                       
089800               MOVE RAETT-1 (INDX)       TO MOD-TEMFSINF                  
089900             END-IF                                                       
090000             EVALUATE TRUE                                                
090100             WHEN MID-IDTRANS-START =  '4315'                             
090200               MOVE 'W4O315N1'         TO MFS-IDMOD                       
090300               MOVE '4317'             TO MFS-IDTRANS                     
090310               MOVE WS-KDMFSFOR        TO MFS-KDMFSFOR                    
090400               COMPUTE MSG-KVLL = LENGTH OF 4315-MOD-W4O31501 + 17        
090500               PERFORM HB-LADDA-4315                                      
090600               MOVE 4315-MOD           TO MSG-AREA                        
090700             WHEN MID-IDTRANS-START =  '4314'                             
090800               MOVE 'W4O314N1'         TO MFS-IDMOD                       
090900               MOVE '4317'             TO MFS-IDTRANS                     
090910               MOVE WS-KDMFSFOR        TO MFS-KDMFSFOR                    
091000               COMPUTE MSG-KVLL = LENGTH OF 4314-MOD-W4O31401 + 17        
091100               PERFORM HA-LADDA-4314                                      
091200               MOVE 4314-MOD           TO MSG-AREA                        
091300             WHEN MID-IDTRANS-START =  '4313'                             
091500               MOVE '4313'             TO MFS-IDTRANS                     
091600               MOVE 'W4T313 '          TO PTOP1-TRANSKOD                  
091900               MOVE WS-KDMFSFOR        TO PTOP1-KDMFSFOR                  
092200              COMPUTE PTOP1-LL = LENGTH OF PTOP1-MID-W4I31301 + 17        
092600             WHEN OTHER                                                   
092700               MOVE '4317'             TO MFS-IDTRANS                     
092800               MOVE MOD-W4O31701       TO MSG-AREA                        
092810               MOVE WS-KDMFSFOR        TO MFS-KDMFSFOR                    
092900             END-EVALUATE                                                 
093000         END-EVALUATE                                                     
093100     END-IF                                                               
093200     .                                                                    
093300     EJECT                                                                
093400 HA-LADDA-4314        SECTION.                                            
093500     SKIP3                                                                
093600     MOVE LOW-VALUE                TO 4314-MOD                            
093700     MOVE '4314'                   TO 4314-MOD-IDTRANS                    
093800                                      4314-MOD-IDTRANS-START              
093900     MOVE SPACE                    TO 4314-MOD-TEMFSFEL                   
094000     MOVE MFS-RENSA-FAELT          TO 4314-MOD-IDANSTNR-IN                
094100                                      4314-MOD-IDDISTR-IN                 
094200                                      4314-MOD-IDKUNDNR-IN                
094300                                      4314-MOD-IDORDNR-IN                 
094400                                      4314-MOD-IDKOLLI-IN                 
094500                                      4314-MOD-IDPRODNR-IN                
094600                                      4314-MOD-FLFORTSK                   
094700                                      4314-MOD-FLSISTAK                   
094800                                      4314-MOD-IDRADNR-FOM-S              
094900                                      4314-MOD-IDRADNR-TOM-S              
095000                                      4314-MOD-KVLEVART-S                 
095100     MOVE WS-IDANSTNR              TO 4314-MOD-IDANSTNR-UT                
095200     INSPECT 4314-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
095300     MOVE WS-IDDISTR               TO 4314-MOD-IDDISTR-UT                 
095400     INSPECT 4314-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
095500     MOVE WS-IDKUNDNR              TO 4314-MOD-IDKUNDNR-UT                
095600     INSPECT 4314-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
095700     MOVE WS-IDORDNR               TO 4314-MOD-IDORDNR-UT                 
095800     INSPECT 4314-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
095900     MOVE WS-IDKOLLI               TO 4314-MOD-IDKOLLI-UT                 
096000     INSPECT 4314-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
096100     MOVE WS-IDPRODNR              TO 4314-MOD-IDPRODNR-UT                
096200     INSPECT 4314-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
096300     MOVE MOD-TEMFSINF             TO 4314-MOD-TEMFSINF                   
096400     SKIP2                                                                
096500     MOVE +1                       TO INX                                 
096600     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL-PLUS-1                         
096700         MOVE MFS-RENSA-FAELT      TO 4314-MOD-IDRADNR-FOM (INX)          
096800                                      4314-MOD-IDRADNR-TOM (INX)          
096900                                      4314-MOD-KVLEVART (INX)             
097000         ADD +1 TO INX                                                    
097100     END-PERFORM                                                          
097200     .                                                                    
097300     EJECT                                                                
097400 HB-LADDA-4315        SECTION.                                            
097500     SKIP3                                                                
097600     MOVE LOW-VALUE                TO 4315-MOD                            
097700     MOVE '4315'                   TO 4315-MOD-IDTRANS                    
097800                                      4315-MOD-IDTRANS-START              
097900     MOVE SPACE                    TO 4315-MOD-TEMFSFEL                   
098000     MOVE MFS-RENSA-FAELT          TO 4315-MOD-IDANSTNR-IN                
098100                                      4315-MOD-IDDISTR-IN                 
098200                                      4315-MOD-IDKUNDNR-IN                
098300                                      4315-MOD-IDORDNR-IN                 
098400                                      4315-MOD-IDKOLLI-IN                 
098500                                      4315-MOD-IDPRODNR-IN                
098600                                      4315-MOD-FLSISTAK                   
098700                                      4315-MOD-KDKOLLI                    
098800                                      4315-MOD-VKORDBTO-KOLLI             
098900                                      4315-MOD-KDEMBTYP                   
099000                                      4315-MOD-DIKOLLIL                   
099100                                      4315-MOD-DIKOLLIB                   
099200                                      4315-MOD-DIKOLLIH                   
099300                                      4315-MOD-ADFLGEO                    
099400                                      4315-MOD-ADFLOMR                    
099500                                      4315-MOD-ADRUTNIV                   
099600                                      4315-MOD-IDKOLLI-FOM                
099700                                      4315-MOD-IDKOLLI-TOM                
099800     MOVE MSGI-KDPRTVAL-ADR        TO 4315-MOD-PRTVAL-ADRESSFL            
099900     MOVE MSGI-KDPRTVAL-FS         TO 4315-MOD-PRTVAL-FOLJEFL             
100000                                                                          
100100     MOVE WS-IDANSTNR              TO 4315-MOD-IDANSTNR-UT                
100200     INSPECT 4315-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
100300     MOVE WS-IDDISTR               TO 4315-MOD-IDDISTR-UT                 
100400     INSPECT 4315-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
100500     MOVE WS-IDKUNDNR              TO 4315-MOD-IDKUNDNR-UT                
100600     INSPECT 4315-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
100700     MOVE WS-IDORDNR               TO 4315-MOD-IDORDNR-UT                 
100800     INSPECT 4315-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
100900     MOVE WS-IDKOLLI               TO 4315-MOD-IDKOLLI-UT                 
101000     INSPECT 4315-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
101100     MOVE WS-IDPRODNR              TO 4315-MOD-IDPRODNR-UT                
101200     INSPECT 4315-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
101300     MOVE MOD-TEMFSINF             TO 4315-MOD-TEMFSINF                   
101400     MOVE +1                       TO INX                                 
101500     PERFORM UNTIL INX NOT < MAX-RAD-ANTAL-PLUS-1                         
101600         MOVE MFS-RENSA-FAELT      TO 4315-MOD-IDRADNR-FOM (INX)          
101700                                      4315-MOD-IDRADNR-TOM (INX)          
101800                                      4315-MOD-KVLEVART (INX)             
101900         ADD +1 TO INX                                                    
102000     END-PERFORM                                                          
102100     .                                                                    
102200     EJECT                                                                
102300 HC-LADDA-4318        SECTION.                                            
102400                                                                          
102500     MOVE LOW-VALUE                TO 4318-MID                            
102600     MOVE WS-IDANSTNR              TO 4318-MID-IDANSTNR-IN                
102700     MOVE SPACE                    TO 4318-MID-IDANSTNR-UT                
102800     MOVE WS-IDDISTR               TO 4318-MID-IDDISTR-IN                 
102900     MOVE SPACE                    TO 4318-MID-IDDISTR-UT                 
103000     MOVE WS-IDKUNDNR              TO 4318-MID-IDKUNDNR-IN                
103100     MOVE SPACE                    TO 4318-MID-IDKUNDNR-UT                
103200     MOVE WS-IDORDNR               TO 4318-MID-IDORDNR-IN                 
103300     MOVE SPACE                    TO 4318-MID-IDORDNR-UT                 
103400     MOVE WS-IDKOLLI               TO 4318-MID-IDKOLLI-IN                 
103500     MOVE SPACE                    TO 4318-MID-IDKOLLI-UT                 
103510     MOVE SPACE                    TO 4318-MID-IDDC-IN                    
103520     MOVE WS-IDDC                  TO 4318-MID-IDDC-UT                    
103600     MOVE WS-IDPRODNR              TO 4318-MID-IDPRODNR-IN                
103700     MOVE SPACE                    TO 4318-MID-IDPRODNR-UT                
103800     MOVE MID-IDTRANS-START        TO 4318-MID-IDTRANS-START              
103900     MOVE '+'                      TO 4318-MID-FLSVAR                     
103910     MOVE 'N'                      TO 4318-MID-FLAGGA                     
104000     .                                                                    
104100     SKIP2                                                                
105500* MFS SEKTIONER                                                           
105600                                                                          
105700 S01-ROER-EJ-MOD-FALT SECTION.                                            
105800                                                                          
105900     MOVE MFS-ROER-EJ-FAELT               TO   MOD-IDANSTNR-IN            
106000                                               MOD-IDDISTR-IN             
106100                                               MOD-IDKUNDNR-IN            
106200                                               MOD-IDORDNR-IN             
106300                                               MOD-IDKOLLI-IN             
106400                                               MOD-IDPRODNR-IN            
106500                                               MOD-IDTRANS-START          
106600                                               MOD-FLSISTAK               
106700                                               MOD-IDRADNR-S              
106800                                               MOD-KDARTURS-S             
106900     MOVE +1 TO RAD-INX                                                   
107000     MOVE +14 TO MAX-RAD-ANTAL                                            
107100     PERFORM UNTIL RAD-INX NOT < MAX-RAD-ANTAL                            
107200         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDRADNR (RAD-INX)              
107300                                       MOD-KDARTURS (RAD-INX)             
107400         ADD +1 TO RAD-INX                                                
107500     END-PERFORM                                                          
107600     .                                                                    
107700     EJECT                                                                
107800 S02-RENSA-MOD-FAELT  SECTION.                                            
107900     SKIP3                                                                
108000     MOVE MFS-RENSA-FAELT                 TO   MOD-IDTRANS-START          
108100                                               MOD-FLSISTAK               
108200     SKIP2                                                                
108300     MOVE +1 TO RAD-INX                                                   
108400     MOVE +14 TO MAX-RAD-ANTAL                                            
108500     PERFORM UNTIL RAD-INX NOT < MAX-RAD-ANTAL                            
108600         MOVE MFS-RENSA-FAELT       TO MOD-IDRADNR (RAD-INX)              
108700                                       MOD-KDARTURS (RAD-INX)             
108800         ADD +1 TO RAD-INX                                                
108900     END-PERFORM                                                          
109000     .                                                                    
109100     EJECT                                                                
109200 S04-ADD-LAES-IN-FAELT  SECTION.                                          
109300     SKIP3                                                                
109400     MOVE +1 TO RAD-INX                                                   
109500     PERFORM UNTIL RAD-INX NOT < MAX-RAD-ANTAL                            
109600         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
109700                                 MOD-IDRADNR-ATTR (RAD-INX)               
109800                                 MOD-KDARTURS-ATTR (RAD-INX)              
109900         ADD +1 TO RAD-INX                                                
110000     END-PERFORM                                                          
110100     .                                                                    
110200     EJECT                                                                
110300* IMS SEKTIONER                                                           
110400     SKIP3                                                                
110500 IMS-GET-MSG SECTION.                                                     
110600                                                                          
110700     MOVE '  QC' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     SKIP3                                                                
111200     .                                                                    
111300 IMS-INSERT-ALT0605MSG SECTION.                                           
111400                                                                          
111500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111600     MOVE SPACE TO GODK-STATUSKODER                                       
111700     CALL CBLTDLI USING ISRT ALT0605-PCB MSG-IO-AREA                      
111800     MOVE ALT0605-STATUS-CODE TO STATUS-WS                                
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     EJECT                                                                
112200 IMS-INSERT-ALT4313-MSG SECTION.                                          
112300                                                                          
112400     MOVE LOW-VALUE TO PTOP1-Z1 PTOP1-Z2                                  
112500     MOVE SPACE TO GODK-STATUSKODER                                       
112600     CALL CBLTDLI USING ISRT                                              
112700                        ALT4313-PCB                                       
112810                        P-TO-P-SW1                                        
112900     MOVE ALT4313-STATUS-CODE TO STATUS-WS                                
113000     PERFORM IMS-STATUSKONTROLL                                           
113100     SKIP3                                                                
113200     .                                                                    
113300 IMS-INSERT-MSG SECTION.                                                  
113400                                                                          
113500     IF NOT ENGLISH-TEXT                                                  
113600       MOVE '0' TO MFS-KDHUVOMR                                           
113700     END-IF                                                               
113800*                                                                         
113900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
114000     MOVE SPACE TO GODK-STATUSKODER                                       
114100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
114200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     SKIP3                                                                
114500     .                                                                    
114600 IMS-CHANGE-ALTMSG       SECTION.                                         
114700     SKIP2                                                                
114800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
114900     MOVE '  ' TO GODK-STATUSKODER                                        
115000     CALL CBLTDLI USING CHNG                                              
115100                          ALT-PCB                                         
115200                          MSG-KDTRANS-1                                   
115300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     EJECT                                                                
115700 IMS-INSERT-ALTMSG SECTION.                                               
115800                                                                          
115810     IF NOT ENGLISH-TEXT                                                  
115820       MOVE '0' TO MFS-KDHUVOMR                                           
115830     END-IF                                                               
115840*                                                                         
115900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
116000     MOVE SPACE TO GODK-STATUSKODER                                       
116100     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
116200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
116300     PERFORM IMS-STATUSKONTROLL                                           
116400     .                                                                    
116500     EJECT                                                                
116620 IMS-GU-KUNDORDER SECTION.                                                
116700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
116800            DELIMITED BY SIZE INTO SSA1                                   
116900     MOVE '  GE' TO GODK-STATUSKODER                                      
117000     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1                  
117100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     SKIP3                                                                
117400     .                                                                    
117500 IMS-GU-RAD       SECTION.                                                
117600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
117700            DELIMITED BY SIZE INTO SSA1                                   
117800     STRING 'WDE411  (IDPURAD  =' W-WDE420-KEYSEQ-X ')'                   
117900            DELIMITED BY SIZE INTO SSA2                                   
118000     MOVE '  GE' TO GODK-STATUSKODER                                      
118100     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1 SSA2             
118200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
118300     PERFORM IMS-STATUSKONTROLL                                           
118400     SKIP3                                                                
118500     .                                                                    
118600 IMS-GU-IDPRODNR   SECTION.                                               
118700     STRING 'WDE601  (IDPRODNR =' W-WDE601KY-X ')'                        
118710            DELIMITED BY SIZE INTO SSA1                                   
118800     MOVE '  GE' TO GODK-STATUSKODER                                      
118900     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-WDE601 SSA1                   
119000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     EJECT                                                                
119300     .                                                                    
119400 IMS-GU-KUNDORDER-SEK SECTION.                                            
119500     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
119600            DELIMITED BY SIZE INTO SSA1                                   
119700     MOVE '  GE' TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GU    WDE4A-PCB DLI-IO-AREA SSA1                  
119900     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
120000                               STATUS-KUNDORDER-SEK-WS                    
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     SKIP3                                                                
120300     .                                                                    
120400 IMS-GN-KUNDORDER-SEK SECTION.                                            
120500     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
120600            DELIMITED BY SIZE INTO SSA1                                   
120700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
120800     CALL CBLTDLI USING GN    WDE4A-PCB DLI-IO-AREA SSA1                  
120900     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
121000                               STATUS-KUNDORDER-SEK-WS                    
121100     PERFORM IMS-STATUSKONTROLL                                           
121200     .                                                                    
121300     EJECT                                                                
121400 IMS-GHU-RAD-SEK  SECTION.                                                
121500     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                    
121600            DELIMITED BY SIZE INTO SSA1                                   
121700     MOVE '    ' TO GODK-STATUSKODER                                      
121800     CALL CBLTDLI USING GHU    WDE4B-PCB DLI-IO-AREA SSA1                 
121900     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
122000     PERFORM IMS-STATUSKONTROLL                                           
122100     SKIP3                                                                
122200     .                                                                    
122300 IMS-REPL-RAD           SECTION.                                          
122400     MOVE '    ' TO GODK-STATUSKODER                                      
122500     CALL CBLTDLI USING REPL WDE4B-PCB DLI-IO-AREA                        
122600     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     EJECT                                                                
123000 IMS-STATUSKONTROLL SECTION.                                              
123100     SET STATUS-IX TO 1                                                   
123200     SEARCH GODK-STATUS AT END CALL FELLOG                                
123300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
123400     END-SEARCH                                                           
123500     .                                                                    
