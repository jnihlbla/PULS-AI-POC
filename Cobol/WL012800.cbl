000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL012800.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   JUN.  2004.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800* WL012800 PROGRAM IS A REPLICA OF W4033300 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*                                                                         
001200*    FUNKTION.                                                            
001300*        PROGRAMMET ANROPAS FRÅN EN WEBBEN OCH SVARAR GENOM               
001400*        ATT SKICKA TILLBAKA PRINT-OUTPUT (OREDIGERAT) SOM DÄR            
001500*        SEDAN REDIGERAS TILL EN KOLLIFLAGGA I HTML-FORMAT.               
001600*                                                                         
001700*        ADDRESS: CARPARTS.LDC.PRCASELABEL                                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: WL0128T                                             
002100*        REQUEST:     WZ01REQU                                            
002200*                     WL0128I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        RESPONSE:    WZ01RESP                                            
002600*                     WL0128O1                                            
002700*                                                                         
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100                                                                          
003200     EJECT                                                                
003300 WORKING-STORAGE           SECTION.                                       
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'WL012800'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
003900 77  ERROR-TEXT                  PIC X(64) VALUE SPACE.                   
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100*                                                                         
004200 77  WS-INDATA-SW                PIC X(1).                                
004300     88  INDATA-OK               VALUE 'J'.                               
004400     88  INDATA-NOT-OK           VALUE 'N'.                               
004500                                                                          
004600 77  FELTEXT                     PIC X(16)   VALUE SPACE.                 
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-SPAR-KVORDRAD-LEVPL      PIC S9(5)   VALUE ZERO COMP-3.           
005000 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
005100 77  WS-KDORDKL                  PIC X(1).                                
005200 77  WS-KDFRAKT                  PIC S9(3)   VALUE ZERO  COMP-3.          
005300 77  WS-ORAD-BERADREF            PIC X(10)   VALUE SPACE.                 
005400 77  WS-KOLLI-KDKOLLI            PIC X(8)    VALUE SPACE.                 
005500 77  WS-KOLLI-IDTRPTNR           PIC 9(3)    VALUE ZERO.                  
005600*                                                                         
005700 77  WS-KOLLI-ADFLGEO            PIC X(3).                                
005800 77  WS-KOLLI-ADFLOMR            PIC 9(3).                                
005900 77  WS-CNT                      PIC S9(5)   VALUE ZERO COMP-3.           
006000 77  WS-CASE-CNT                 PIC S9(5)   VALUE ZERO COMP-3.           
006100 77  WS-REC-NUM                  PIC S9(5)   VALUE ZERO COMP-3.           
006200 77  WS-REC-LIMIT                PIC X(01).                               
006300   88  REC-LIMIT                             VALUE 'N'.                   
006400 77  WS-TID                      PIC 9(4).                                
006500*                                                                         
006600 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
006700                                                                          
006800 77  WS-IDELMT-ERROR             PIC X(16).                               
006900 77  WS-IDMSG-ERROR              PIC X(03).                               
007000 77  WS-IDMSG-INFO               PIC X(03).                               
007100                                                                          
007200*                                                                         
007300 77    IDPRODNR-IFYLLT-SW        PIC X(01).                               
007400   88  IDPRODNR-IFYLLT                       VALUE 'J'.                   
007500 77    SW-ADRESS-HAMTAD          PIC X(01).                               
007600   88  ADRESS-INTE-HAMTAD                    VALUE 'N'.                   
007700*                                                                         
007800 77  WS-SLINGA-KLAR              PIC X(1).                                
007900     88  SLINGA-KLAR             VALUE 'J'.                               
008000                                                                          
008100 01  UNICODE-SPACE               PIC X VALUE X'20'.                       
008200 01  WS-BARCODE.                                                          
008300     03  WS-BARCODE-DISTR        PIC  9(4).                               
008400     03  WS-BARCODE-KUNDNR       PIC  9(6).                               
008500     03  WS-BARCODE-ORDNR        PIC  9(7).                               
008600     03  WS-BARCODE-KOLLI        PIC  9(5).                               
008700*                                                                         
008800 01  WS-DARFS                    PIC  9(12).                              
008900 01  FILLER REDEFINES  WS-DARFS.                                          
009000     03  FILLER                  PIC  9(2).                               
009100     03  WS-DARFS-YYMMDD         PIC  9(6).                               
009200     03  WS-DARFS-HHMM           PIC  9(4).                               
009300                                                                          
009400 01  WS-OHUV-IDDEPT              PIC 9(2)  VALUE ZERO.                    
009500 77  FILLER                      PIC X(8)    VALUE 'ARBFAELT'.            
009600*                                                                         
009700 01  ARBETSFAELT.                                                         
009800   03  WS-IDDISTR                           PIC 9(4)  VALUE ZERO.         
009900   03  WS-IDKUNDNR                          PIC X(6)  VALUE SPACE.        
010000   03  WS-IDKUNDNR-7                        PIC 9(7)  VALUE ZERO.         
010100   03  WS-IDORDNR                           PIC X(5)  VALUE SPACE.        
010200   03  WS-IDPRODNR                          PIC X(7)  VALUE SPACE.        
010300   03  WS-IDPRODNR-7                        PIC 9(7)  VALUE ZERO.         
010400   03  WS-IDKOLLI                           PIC X(5)  VALUE SPACE.        
010500   03  WS-IDKOLLI-PRT                       PIC 9(5)  VALUE ZERO.         
010600   03  WS-IDKOLLI-TOM                       PIC 9(5)  VALUE ZERO.         
010700   03  WS-IDKOLLI-FLER                      PIC S9(5) COMP-3.             
010800   03  WS-TAKF-BEGMT-RAD1                   PIC X(35) VALUE SPACE.        
010900   03  WS-TAKF-BEGMT-RAD2                   PIC X(35) VALUE SPACE.        
011000                                                                          
011100 01  WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                   
011200                                                                          
011300 01  GENERELLA-SUBPROGRAM.                                                
011400   03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.               
011500   03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.               
011600   03 ABEND                    PIC X(8)   VALUE 'ABEND   '.               
011700   03 WZ01SUB                  PIC X(8)   VALUE 'WZ01SUB '.               
011800   03 WTRAUTF8                 PIC X(8)   VALUE 'WTRAUTF8'.               
011900     EJECT                                                                
012000                                                                          
012100 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
012200*01  -COPY WTRAUTF8                                                       
012300*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
012400*                                                                         
012500 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
012600*                                                                         
012700 01  FILLER                      PIC X(16)  VALUE 'WZ01SUB  '.            
012800*   -COPY WZ01SUB                                                         
012900*                                                                         
013000 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
013100*                                                                         
013200 01  REQU-AREA.                                                           
013300*    03 -COPY WZ01REQU                                                    
013400*    03 -COPY WL0128I1                                                    
013500*                                                                         
013600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
013700*                                                                         
013800 01  RESP-AREA.                                                           
013900*    03 -COPY WZ01RESP                                                    
014000*    03 -COPY WL0128O1                                                    
014100                                                                          
014200     EJECT                                                                
014300*****************************************************************         
014400*         AREA MED STYRTECKEN FÖR ERICSSON XXXX RAD-SKRIVARE.   *         
014500*         ANV. FÖR ATT SKRIVA KOLLIFLAGGA I A4 FORMAT I CDC.    *         
014600*****************************************************************         
014700 01  FILLER                      PIC X(16)  VALUE 'LISTA-RAD'.            
014800 01  RADREDIGERING.                                                       
014900       05  STOR-RAD2.                                                     
015000           07  LISTA-KDFRAKT              PIC Z9    VALUE ZERO.           
015100       05  STOR-RAD3.                                                     
015200           07  LISTA-TIRFS                PIC 9(6)  VALUE ZERO.           
015300       05  LITEN-RAD.                                                     
015400           07  LISTA-ADRUTNIV             PIC Z(2)9 VALUE ZERO.           
015500     EJECT                                                                
015600******************************************************************        
015700 01  FILLER                     PIC X(16) VALUE 'DISTR-COPY-TEXT'.        
015800 01     TEST-IDDISTR            PIC 9(5)              COMP-3.             
015900 01     FILLER REDEFINES TEST-IDDISTR.                                    
016000*  03   -COPY WWDIST03.                                                   
016100     SKIP2                                                                
016200 01     FILLER REDEFINES TEST-IDDISTR.                                    
016300*  03   -COPY WWDIST05.                                                   
016400     SKIP2                                                                
016500 01     FILLER REDEFINES TEST-IDDISTR.                                    
016600*  03   -COPY WWDIST13.                                                   
016700     SKIP2                                                                
016800 01     FILLER REDEFINES TEST-IDDISTR.                                    
016900*  03   -COPY WWDIST30.                                                   
017000     SKIP2                                                                
017100 01     FILLER REDEFINES TEST-IDDISTR.                                    
017200*  03   -COPY WWDIST85.                                                   
017300     SKIP2                                                                
017400                                                                          
017500******************************************************************        
017600 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-DLI'.          
017700*                                                                         
017800 01  NYCKLAR-TILL-DLI.                                                    
017900   03  W-IDGMT-X.                                                         
018000     05  W-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
018100     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO  COMP-3.          
018200*                                                                         
018300     03  W-IDGMTREF-X.                                                    
018400         05  W-IDGMTREF          PIC X(17)        VALUE SPACE.            
018500*                                                                         
018600   03    W-WDE4E1KY-MAX-X.                                                
018700     05    W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
018800     05    W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.            
018900                                                                          
019000   03    W-WDE4E1KY-MIN-X.                                                
019100     05    W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
019200     05    W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.             
019300                                                                          
019400   03 W-IDPRODNR-WDE611-X.                                                
019500     05  W-IDPRODNR-WDE611     PIC S9(7)   VALUE ZERO  COMP-3.            
019600   03 W-IDKOLLI-WDE611-X.                                                 
019700     05  W-IDKOLLI-WDE611      PIC S9(5)   VALUE ZERO  COMP-3.            
019800                                                                          
019900   03 W-WDE421KY-X.                                                       
020000     05  W-IDPRODNR-WDE421     PIC S9(7)   VALUE ZERO  COMP-3.            
020100     05  W-IDKOLLI-WDE421      PIC S9(5)   VALUE ZERO  COMP-3.            
020200                                                                          
020300                                                                          
020400     EJECT                                                                
020500   03  W-WDB501KY-X.                                                      
020600     05  W-501-IDDC              PIC X(2).                                
020700     05  W-501-KDFRAKT           PIC S9(3)   VALUE ZERO  COMP-3.          
020800     05  W-501-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
020900     05  W-501-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
021000*                                                                         
021100   03  W-WDB501KY-DEFAULT-X.                                              
021200     05  W-501-IDDC-DEFAULT      PIC X(2).                                
021300     05  W-501-KDFRAKT-DEFAULT   PIC S9(3)   VALUE ZERO    COMP-3.        
021400     05  W-501-IDDISTR-DEFAULT   PIC S9(5)   VALUE ZERO    COMP-3.        
021500     05  W-501-IDKUNDNR-DEFAULT  PIC S9(7)   VALUE 9999999 COMP-3.        
021600*                                                                         
021700   03  W-KUNDORDER-SEK-X.                                                 
021800     05  W-4A1-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
021900     05  W-4A1-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
022000     05  W-4A1-IDKUNDRF.                                                  
022100       07  W-4A1-IDORDNR         PIC X(5).                                
022200       07  FILLER                PIC X(5).                                
022300*                                                                         
022400   03  W-KUNDORDER-X.                                                     
022500     05  W-401-IDDISTR           PIC S9(5)   VALUE ZERO  COMP-3.          
022600     05  W-401-IDKUNDNR          PIC S9(7)   VALUE ZERO  COMP-3.          
022700     05  W-401-IDKUNDRF.                                                  
022800       07  W-401-IDORDNR         PIC X(5).                                
022900       07  FILLER                PIC X(5).                                
023000     05  W-401-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
023100     05  W-401-IDPLKLST          PIC S9(3)   VALUE ZERO  COMP-3.          
023200*                                                                         
023300   03  W-IDPURAD-X.                                                       
023400     05  W-IDPURAD               PIC S9(5)   VALUE 00001 COMP-3.          
023500*                                                                         
023600   03  W-WDQ301KY-X.                                                      
023700     05  W-301-IDORDER           PIC S9(7)   VALUE ZERO  COMP-3.          
023800     05  W-301-IDDC              PIC X(2)    VALUE SPACE.                 
023900     05  W-301-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
024000     05  W-301-IDPLKLST          PIC S9(3)   VALUE ZERO  COMP-3.          
024100*                                                                         
024200   03  W-IDDC-X.                                                          
024300     05  W-IDDC                  PIC X(2).                                
024400*                                                                         
024500   03  W-KDFRAKT-X.                                                       
024600     05  W-KDFRAKT               PIC S9(3)   VALUE ZERO  COMP-3.          
024700*                                                                         
024800   03  W-IDPRODNR-X.                                                      
024900     05  W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.          
025000*                                                                         
025100   03  W-IDKOLLI-X.                                                       
025200     05  W-IDKOLLI               PIC S9(5)   VALUE ZERO  COMP-3.          
025300*                                                                         
025400   03  W-IDORDER-X.                                                       
025500      05 W-IDORDER               PIC S9(7)   COMP-3.                      
025600*                                                                         
025700   03  W-IDARTNR-X.                                                       
025800      05 W-IDARTNR               PIC S9(9)   COMP-3.                      
025900*    --- PARAMETERS TO ABEND                                              
026000                                                                          
026100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
026200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
026300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
026400******************************************************************        
026500     EJECT                                                                
026600 01  MEDDELANDE.                                                          
026700   03  FEL-X-NOT-FOUND           PIC X(3)  VALUE '025'.                   
026800   03  FEL-ORDER                 PIC X(16) VALUE 'IDORDNR'.               
026900   03  FEL-KVRADER               PIC X(16) VALUE 'KVRADER'.               
027000   03  FEL-IDKOLLI               PIC X(16) VALUE 'IDKOLLI'.               
027100   03  FEL-NO-APPROVED-CASE      PIC X(3)  VALUE '133'.                   
027200   03  FEL-LINE-NOT-FOUND        PIC X(3)  VALUE '027'.                   
027300   03  FEL-X-MUST-BE-ENTERED     PIC X(3)  VALUE '026'.                   
027400   03  FEL-DC                    PIC X(16) VALUE 'IDDC'.                  
027500   03  FEL-IDKUNDNR              PIC X(16) VALUE 'IDKUNDNR'.              
027600   03  FEL-IDDISTR               PIC X(16) VALUE 'IDDISTR'.               
027700   03  FEL-KDFRAKT               PIC X(16) VALUE 'KDFRAKT'.               
027800******************************************************************        
027900*                                                                         
028000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028100*                                                                         
028200 01  IMS-WS.                                                              
028300   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
028400     SKIP3                                                                
028500*                        **** STATUS-KOD FRÅN IMS                         
028600   03  STATUS-KUNDORDER-SEK-WS   PIC XX.                                  
028700     88  KUNDORDER-SEK-FINNS                 VALUE '  '.                  
028800     88  KUNDORDER-SEK-SAKNAS                VALUE 'GE' 'GB'.             
028900   03  STATUS-WS                 PIC XX.                                  
029000     88  SEGMENT-FINNS                       VALUE '  '.                  
029100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029200     SKIP3                                                                
029300   03  GODK-STATUSKODER.                                                  
029400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-INDX PIC XX.              
029500     SKIP3                                                                
029600 01    SSA1                      PIC X(96).                               
029700 01    SSA2                      PIC X(64).                               
029800     EJECT                                                                
029900*                            IMS FUNKTIONSKODER                           
030000*01    -COPY W0003                                                        
030100     EJECT                                                                
030200*                            IDDC COPYBOOK                                
030300*01    -COPY WWDC99                                                       
030400     EJECT                                                                
030500*                            DLI INPUT-OUTPUT AREA                        
030600 01  DLI-IO-AREA.                                                         
030700   03  IO-AREA                   PIC X(320)  VALUE SPACE.                 
030800     SKIP3                                                                
030900*  03  WDE401    -COPY WDE401  -RED IO-AREA.                              
031000     EJECT                                                                
031100 01  DLI-IO-AREA3.                                                        
031200   03  IO-AREA3                  PIC X(288)  VALUE SPACE.                 
031300     SKIP3                                                                
031400*  03  WLORQA01  -COPY WDQ301  -RED IO-AREA3.                             
031500     EJECT                                                                
031600 01  FILLER                      PIC X(16)  VALUE 'Q201-AREA'.            
031700 01  DLI-IO-Q201.                                                         
031800*  03  -COPY WDQ201                                                       
031900     EJECT                                                                
032000 01  FILLER                      PIC X(16)  VALUE 'Q212-AREA'.            
032100 01  DLI-IO-Q212.                                                         
032200*  03  -COPY WDQ212                                                       
032300     EJECT                                                                
032400 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
032500*01  -COPY WDB201                                                         
032600     EJECT                                                                
032700 01  FILLER                      PIC X(16)  VALUE 'WDB5-AREA'.            
032800*01  -COPY WDB501                                                         
032900     EJECT                                                                
033000 01  FILLER                   PIC X(16) VALUE 'DLI-AREA-WDE601'.          
033100 01  DLI-IOAREA-WDE601.                                                   
033200*  03  WDE601    -COPY WDE601                                             
033300*                                                                         
033400 01  FILLER                   PIC X(16) VALUE 'DLI-AREA-WDE611'.          
033500 01  DLI-IOAREA-WDE611.                                                   
033600*     03            -COPY WDE611                                          
033700     EJECT                                                                
033800 01  FILLER                   PIC X(16) VALUE 'DLI-AREA-WDE411'.          
033900 01  DLI-IO-WDE411-01.                                                    
034000*     03            -COPY WDE411                                          
034010*     03            -COPY WDE401 -PRE FSEQ-                               
034100 01  FILLER                   PIC X(16) VALUE 'DLI-AREA-WDE421'.          
034200 01  DLI-IOAREA-WDE421.                                                   
034300*     03            -COPY WDE421                                          
034400     EJECT                                                                
034500 01  FILLER                   PIC X(16) VALUE 'DLI-AREA-WDE4E1'.          
034600 01  DLI-IOAREA-WDE4E1.                                                   
034700*     03            -COPY WDE4E1                                          
034800     EJECT                                                                
034900 01  FILLER                      PIC X(16)   VALUE 'WDI2-AREA'.           
035000 01   DLI-IO-WDI2.                                                        
035100      03  -COPY WDI201.                                                   
035200     EJECT                                                                
035300 LINKAGE                   SECTION.                                       
035400 01  MSG-PCB                     PIC X.                                   
035500     SKIP2                                                                
035600*01  -COPY W0008     -PRE WDE42-                                          
035700     05  FILLER                  PIC X.                                   
035800     SKIP2                                                                
035900*01  -COPY W0008     -PRE WDE4A-                                          
036000     05  FILLER                  PIC X.                                   
036100     EJECT                                                                
036200*01  -COPY W0008     -PRE ORQI-                                           
036300     05  FILLER                  PIC X.                                   
036400     SKIP2                                                                
036500*01  -COPY W0008     -PRE GMTA-                                           
036600     05  FILLER                  PIC X.                                   
036700     SKIP2                                                                
036800*01  -COPY W0008     -PRE ORQA-                                           
036900     05  FILLER                  PIC X.                                   
037000     SKIP2                                                                
037100*01  -COPY W0008     -PRE GMTC-                                           
037200     05  FILLER                  PIC X.                                   
037300     EJECT                                                                
037400*01  -COPY W0008     -PRE WDE6-                                           
037500     05  FILLER                  PIC X.                                   
037600     EJECT                                                                
037700*01  -COPY W0008     -PRE ORDD-                                           
037800     05  FILLER                  PIC X.                                   
037900     EJECT                                                                
038000*01  -COPY W0008     -PRE WDE4-                                           
038100     05  FILLER                  PIC X.                                   
038200     EJECT                                                                
038300*01  -COPY W0008     -PRE WDE4E-                                          
038400     05  FILLER                  PIC X.                                   
038500     EJECT                                                                
038600*01  -COPY W0008     -PRE WDI2-                                           
038700     05  FILLER                  PIC X.                                   
038800     EJECT                                                                
038900 PROCEDURE DIVISION USING MSG-PCB  WDE42-PCB WDE4A-PCB                    
039000                                   ORQI-PCB  GMTA-PCB                     
039100                                   ORQA-PCB  GMTC-PCB                     
039200                                   WDE6-PCB  ORDD-PCB                     
039300                                   WDE4-PCB  WDE4E-PCB                    
039400                                   WDI2-PCB.                              
039500 MAIN SECTION.                                                            
039600     ENTRY 'DLITCBL' USING MSG-PCB WDE42-PCB WDE4A-PCB                    
039700                                   ORQI-PCB  GMTA-PCB                     
039800                                   ORQA-PCB  GMTC-PCB                     
039900                                   WDE6-PCB  ORDD-PCB                     
040000                                   WDE4-PCB  WDE4E-PCB                    
040100                                   WDI2-PCB.                              
040200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
040300                                                                          
040400     IF SUB-KDRC = 0                                                      
040500       MOVE ALL '+' TO RESP-AREA                                          
040600       MOVE SPACE   TO RESP-IDMSG-INFO                                    
040700                       RESP-IDMSG-ERROR                                   
040800                       RESP-IDELMT-ERROR                                  
040900       MOVE 001     TO RESP-IDMSGVER                                      
041000       MOVE ZERO    TO RESP-REP-KVRADER                                   
041100                                                                          
041200       IF REQU-L128-KVRADER NUMERIC AND REQU-L128-KVRADER > 0             
041300         MOVE 1 TO WS-CNT                                                 
041400         MOVE 'Y' TO WS-REC-LIMIT                                         
041500         MOVE REQU-L128-KVRADER  TO WS-REC-NUM                            
041600         PERFORM UNTIL REC-LIMIT                                          
041700           PERFORM A-INIT                                                 
041800           IF WS-IDDISTR  NUMERIC AND WS-IDKUNDNR NUMERIC AND             
041900*             WS-IDORDNR  NUMERIC AND WS-IDKOLLI  NUMERIC AND             
042000              WS-IDKOLLI  NUMERIC AND                                     
042100              WS-IDPRODNR NUMERIC                                         
042200*             WS-IDPRODNR NUMERIC AND DC-OK                               
042300                                                                          
042400             IF IDPRODNR-IFYLLT                                           
042500             AND WS-IDPRODNR > ZERO                                       
042600*DETTA BEHÖVS OM MAN END. FYLLER I PRODNR.                                
042700                                                                          
042800               PERFORM IMS-GU-ORDD-WDE601                                 
042900               IF SEGMENT-FINNS                                           
043000                 MOVE VORD-IDDISTR     TO WS-IDDISTR                      
043100                                          RESP-L128-IDDISTR-KEY           
043200                 MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR-7                   
043300                 MOVE WS-IDKUNDNR-7(2:6) TO WS-IDKUNDNR                   
043400                                            RESP-L128-IDKUNDNR-KEY        
043500                 MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MAX               
043600                                       RESP-L128-IDPRODNR-KEY             
043700                 MOVE VORD-IDPRODNR TO W-IDPRODNR-WDE4E-MIN               
043800                 PERFORM IMS-GET-WDE4E                                    
043900                 IF SEGMENT-FINNS                                         
044000                   MOVE SEQE-IDORDNR5  TO WS-IDORDNR                      
044100                                          RESP-L128-IDORDNR-KEY           
044200                 END-IF                                                   
044300               END-IF                                                     
044400             END-IF                                                       
044500                                                                          
044600             MOVE 'N'          TO WS-SLINGA-KLAR                          
044700             MOVE WS-IDDISTR   TO W-4A1-IDDISTR                           
044800                                  W-IDDISTR                               
044900             MOVE WS-IDKUNDNR  TO W-4A1-IDKUNDNR                          
045000                                  W-IDKUNDNR                              
045100             MOVE SPACE        TO W-4A1-IDKUNDRF                          
045200             MOVE WS-IDORDNR   TO W-4A1-IDORDNR                           
045300             PERFORM IMS-GU-WDE401-ASEQ                                   
045400                                                                          
045500             IF SEGMENT-FINNS                                             
045600               PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                      
045700                             SLINGA-KLAR                                  
045800                 MOVE KORD-IDDISTR  TO W-401-IDDISTR                      
045900                 MOVE KORD-IDKUNDNR TO W-401-IDKUNDNR                     
046000                 MOVE SPACE         TO W-401-IDKUNDRF                     
046100                 MOVE KORD-IDORDNR5 TO W-401-IDORDNR                      
046200                 MOVE KORD-IDPRODNR TO W-401-IDPRODNR                     
046400                                       W-IDPRODNR-WDE611                  
046500                                       W-IDPRODNR                         
046600                 MOVE KORD-IDPLKLST TO W-401-IDPLKLST                     
046900                 PERFORM IMS-GU-WDE401                                    
047000                 MOVE KORD-KVORDRAD-LEVPL   TO                            
047100                                            WS-SPAR-KVORDRAD-LEVPL        
047200                 MOVE KORD-IDORDER          TO W-IDORDER                  
047400*FIX START - FÖR DIRLEV ORDDEL                                            
047500*DIRLEV                                                                   
047600                 IF IDPRODNR-IFYLLT                                       
047700                 AND WS-IDPRODNR > ZERO                                   
047800                   PERFORM IMS-GU-ORDD-WDE601                             
047900                   IF VORD-IDDC   = REQU-L128-IDDC-KEY (WS-CNT)           
048000                     MOVE 'J' TO WS-SLINGA-KLAR                           
048100                   END-IF                                                 
048200                 ELSE                                                     
048300                   PERFORM IMS-GU-ORDD-WDE601                             
048400                   IF (VORD-IDDC = REQU-L128-IDDC-KEY (WS-CNT) AND        
048500                      WS-SPAR-KVORDRAD-LEVPL = ZERO)                      
048600                     MOVE 'J' TO WS-SLINGA-KLAR                           
048700                   END-IF                                                 
048800                 END-IF                                                   
048900                 IF SLINGA-KLAR                                           
049000                   MOVE VORD-KDFRAKT  TO RESP-L128-KDFRAKT                
049100                                         IN RESP-RAD2                     
049200                   MOVE VORD-KDFRAKT  TO LISTA-KDFRAKT                    
049300                                         WS-KDFRAKT                       
049400                                         W-KDFRAKT                        
049500                                         W-501-KDFRAKT                    
049600                                         W-501-KDFRAKT-DEFAULT            
049700                   MOVE VORD-IDPRODNR TO W-IDPRODNR                       
049800                                         W-IDPRODNR-WDE611                
049900                                         WS-IDPRODNR-7                    
050000                   MOVE WS-IDPRODNR-7        TO WS-IDPRODNR               
050100                   MOVE WS-IDKOLLI TO WS-IDKOLLI-PRT                      
050200                   MOVE +1  TO WS-CASE-CNT                                
050300                   PERFORM UNTIL WS-IDKOLLI-PRT > WS-IDKOLLI-TOM          
050400                              OR WS-CASE-CNT > 100                        
050500                              OR WS-IDKOLLI-PRT = 0                       
050600                              OR INDATA-NOT-OK                            
050700                     MOVE WS-IDKOLLI-PRT TO W-IDKOLLI-WDE611              
050800                     PERFORM IMS-GU-WDE611                                
050900                     IF SEGMENT-FINNS                                     
051000                       IF KOLLI-KDKOLSTA =                                
051100                                 1 OR 2 OR 6 OR 7 OR 8 OR 9               
051200                         PERFORM C-SHOW-INFO                              
051300                         IF ADRESS-INTE-HAMTAD                            
051400                           PERFORM F-HAEMTA-ADRESS                        
051500                           MOVE JA      TO SW-ADRESS-HAMTAD               
051600                         END-IF                                           
051700                         IF RESP-IDMSG-ERROR NOT = SPACE                  
051800                           CONTINUE                                       
051900                         ELSE                                             
052100                           IF REQU-KDPGMACT = 'E'                         
052200                              PERFORM D-PRINT-LIST                        
052300                              ADD +1  TO WS-CASE-CNT                      
052400                           END-IF                                         
052500                         END-IF                                           
052600                       ELSE                                               
052700*                        IF REQU-L128-FLBG = 'Y'                          
052800*                          CALL ABEND USING RKOD-ABEND-WITH-DUMP          
052900*                        ELSE                                             
053000                           MOVE FEL-NO-APPROVED-CASE                      
053100                              TO RESP-IDMSG-ERROR                         
053200*                        END-IF                                           
053300                       END-IF                                             
053400                     ELSE                                                 
053500*                      IF REQU-L128-FLBG = 'Y'                            
053600*                        CALL ABEND USING RKOD-ABEND-WITH-DUMP            
053700*                      ELSE                                               
053800                         MOVE FEL-X-NOT-FOUND TO RESP-IDMSG-ERROR         
053900                         MOVE FEL-IDKOLLI     TO RESP-IDELMT-ERROR        
054000*                      END-IF                                             
054100                     END-IF                                               
054200                     ADD +1 TO WS-IDKOLLI-PRT                             
054300                   END-PERFORM                                            
054400                 END-IF                                                   
054500                 PERFORM IMS-GN-WDE401                                    
054600               END-PERFORM                                                
054700             ELSE                                                         
054800*              IF REQU-L128-FLBG = 'Y'                                    
054900*                CALL ABEND USING RKOD-ABEND-WITH-DUMP                    
055000*              ELSE                                                       
055100                 MOVE FEL-X-NOT-FOUND TO RESP-IDMSG-ERROR                 
055200                 MOVE FEL-ORDER       TO RESP-IDELMT-ERROR                
055300*              END-IF                                                     
055400             END-IF                                                       
055500           ELSE                                                           
055600*            IF REQU-L128-FLBG = 'Y'                                      
055700*              CALL ABEND USING RKOD-ABEND-WITH-DUMP                      
055800*            ELSE                                                         
055900               MOVE '043'               TO RESP-IDMSG-ERROR               
056000*            END-IF                                                       
056100           END-IF                                                         
056200                                                                          
056300*          IF NOT SLINGA-KLAR                                             
056400*            IF REQU-L128-FLBG = 'Y'                                      
056500*              CALL ABEND USING RKOD-ABEND-WITH-DUMP                      
056600*            ELSE                                                         
056700*              MOVE '041'               TO RESP-IDMSG-ERROR               
056800*              MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR              
056900*            END-IF                                                       
057000*          END-IF                                                         
057100                                                                          
057200           IF WS-CNT = WS-REC-NUM OR WS-CNT = 50                          
057300             MOVE 'N' TO WS-REC-LIMIT                                     
057400           ELSE                                                           
057500             ADD 1 TO WS-CNT                                              
057600           END-IF                                                         
057700         END-PERFORM                                                      
057800       ELSE                                                               
057900*        IF REQU-L128-FLBG = 'Y'                                          
058000*          CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
058100*        ELSE                                                             
058200           MOVE '041'               TO RESP-IDMSG-ERROR                   
058300           MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                  
058400*        END-IF                                                           
058500       END-IF                                                             
058600                                                                          
058700       IF REQU-L128-FLBG = 'Y'                                            
058800         MOVE ZERO    TO RESP-L128-IDDISTR-KEY                            
058900                         RESP-L128-IDKUNDNR-KEY                           
059000                         RESP-L128-IDORDNR-KEY                            
059100                         RESP-L128-IDKOLLI-KEY                            
059200                         RESP-L128-IDPRODNR-KEY                           
059300                         RESP-L128-ADFLOMR                                
059400                         RESP-L128-ADRUTNIV                               
059500                         RESP-L128-ADVMODUL                               
059600                         RESP-L128-ADHMODUL                               
059700                         RESP-L128-KDFRAKT                                
059800                                                                          
059900         MOVE SPACES TO  RESP-L128-ADFLGEO                                
060000                         RESP-IDMSG-ERROR                                 
060100                         RESP-IDELMT-ERROR                                
060200                         RESP-IDMSG-INFO                                  
060300       ELSE                                                               
060400         MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                         
060500         MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                        
060600         MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                       
060700         IF WS-IDMSG-ERROR NOT = SPACE                                    
060800           MOVE ALL '+' TO RESP-AREA                                      
060900           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
061000           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
061100           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
061200           MOVE  001             TO RESP-IDMSGVER                         
061300           MOVE ZERO             TO RESP-REP-KVRADER                      
061400         END-IF                                                           
061500       END-IF                                                             
061600       PERFORM S02-RETURN-DATA                                            
061700     END-IF                                                               
061800     MOVE ZERO TO RETURN-CODE                                             
061900                                                                          
062000     GOBACK                                                               
062100     .                                                                    
062200     EJECT                                                                
062300 A-INIT                    SECTION.                                       
062400                                                                          
062500     MOVE 001               TO RESP-IDMSGVER                              
062600     MOVE +00001            TO W-IDPURAD                                  
062700     MOVE NEJ               TO IDPRODNR-IFYLLT-SW                         
062800     MOVE NEJ               TO SW-ADRESS-HAMTAD                           
062900     MOVE SPACE             TO RESP-RAD2                                  
063000                                                                          
063100     IF REQU-L128-IDKOLLI-KEY(1) = ALL '+' OR SPACE                       
063200       MOVE 'IDKOLLI'             TO RESP-IDELMT-ERROR                    
063300       MOVE FEL-X-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
063400       MOVE NEJ                   TO WS-INDATA-SW                         
063500     ELSE                                                                 
063600        IF REQU-L128-IDPRODNR-KEY(1) = ALL '+' OR SPACE                   
063700          MOVE 'IDPRODNR'            TO RESP-IDELMT-ERROR                 
063800          MOVE FEL-X-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                  
063900          MOVE NEJ                   TO WS-INDATA-SW                      
064000        ELSE                                                              
064100          MOVE SPACE                 TO RESP-IDELMT-ERROR                 
064200          MOVE SPACE                 TO RESP-IDMSG-ERROR                  
064300          MOVE JA                    TO WS-INDATA-SW                      
064400        END-IF                                                            
064500     END-IF                                                               
064600                                                                          
064700     IF INDATA-OK                                                         
064800     IF REQU-L128-IDDISTR-KEY (WS-CNT) NUMERIC                            
064900       MOVE REQU-L128-IDDISTR-KEY (WS-CNT) TO WS-IDDISTR                  
065000                                     RESP-L128-IDDISTR-KEY                
065100     ELSE                                                                 
065200     IF REQU-L128-IDDISTR-KEY (WS-CNT) = ALL '+' OR SPACE                 
065300       MOVE ZERO                           TO WS-IDDISTR                  
065400     END-IF                                                               
065500     END-IF                                                               
065600                                                                          
065700     IF REQU-L128-IDKUNDNR-KEY (WS-CNT) NUMERIC                           
065800       MOVE REQU-L128-IDKUNDNR-KEY (WS-CNT)  TO WS-IDKUNDNR               
065900                                       RESP-L128-IDKUNDNR-KEY             
066000     ELSE                                                                 
066100     IF REQU-L128-IDKUNDNR-KEY (WS-CNT) = ALL '+' OR SPACE                
066200*      MOVE ZERO                           TO WS-IDKUNDNR                 
066300       MOVE '000000'                       TO WS-IDKUNDNR                 
066400     END-IF                                                               
066500     END-IF                                                               
066600                                                                          
066700     IF REQU-L128-IDORDNR-KEY (WS-CNT) NUMERIC                            
066800       MOVE REQU-L128-IDORDNR-KEY (WS-CNT) TO WS-IDORDNR                  
066900                                     RESP-L128-IDORDNR-KEY                
067000     ELSE                                                                 
067100     IF REQU-L128-IDKUNDNR-KEY (WS-CNT) = ALL '+' OR SPACE                
067200       MOVE '000000'                       TO WS-IDKUNDNR                 
067300     END-IF                                                               
067400     END-IF                                                               
067500                                                                          
067600     IF REQU-L128-IDKOLLI-KEY (WS-CNT) NUMERIC                            
067700        MOVE REQU-L128-IDKOLLI-KEY (WS-CNT) TO WS-IDKOLLI                 
067800                                               W-IDKOLLI                  
067900                                    RESP-L128-IDKOLLI-KEY                 
068000     END-IF                                                               
068100                                                                          
068200     IF REQU-L128-IDKOLLI-TOM (WS-CNT) = ALL '+' OR SPACE                 
068300                                                                          
068400       MOVE ZERO           TO WS-IDKOLLI-TOM                              
068500     ELSE                                                                 
068600       MOVE REQU-L128-IDKOLLI-TOM (WS-CNT) TO WS-IDKOLLI-TOM              
068700     END-IF                                                               
068800                                                                          
068900     IF WS-IDKOLLI > WS-IDKOLLI-TOM                                       
069000       MOVE WS-IDKOLLI      TO WS-IDKOLLI-TOM                             
069100     ELSE                                                                 
069200       IF WS-IDKOLLI-TOM = 99999                                          
069300         MOVE WS-IDKOLLI    TO WS-IDKOLLI-TOM                             
069400       END-IF                                                             
069500     END-IF                                                               
069600                                                                          
069700     MOVE REQU-L128-IDDC-KEY (WS-CNT) TO RESP-L128-IDDC-KEY               
069800                                         W-IDDC                           
069900                                         W-301-IDDC                       
070000                                         WS-IDDC                          
070100                                                                          
070200*DIRLEV                                                                   
070300     IF REQU-L128-IDPRODNR-KEY (WS-CNT) = ALL '+' OR SPACE                
070400         MOVE ZEROS  TO W-IDPRODNR                                        
070500                          WS-IDPRODNR                                     
070600     ELSE                                                                 
070700       MOVE REQU-L128-IDPRODNR-KEY (WS-CNT) TO  WS-IDPRODNR               
070800       IF REQU-L128-IDPRODNR-KEY (WS-CNT) NUMERIC                         
070900          MOVE REQU-L128-IDPRODNR-KEY (WS-CNT)                            
071000                            TO RESP-L128-IDPRODNR-KEY                     
071100                               W-IDPRODNR                                 
071200       END-IF                                                             
071300       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
071400       MOVE JA              TO IDPRODNR-IFYLLT-SW                         
071500     END-IF                                                               
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 C-SHOW-INFO               SECTION.                                       
072000                                                                          
072100     MOVE KOLLI-ADFLGEO             TO RESP-L128-ADFLGEO                  
072200                                       IN RESP-RAD2                       
072300     MOVE KOLLI-ADFLGEO             TO WS-KOLLI-ADFLGEO                   
072400     MOVE KOLLI-ADFLOMR             TO RESP-L128-ADFLOMR                  
072500                                       IN RESP-RAD2                       
072600     MOVE KOLLI-ADFLOMR             TO WS-KOLLI-ADFLOMR                   
072700     MOVE KOLLI-ADRUTNIV            TO RESP-L128-ADRUTNIV                 
072800                                       IN RESP-RAD2                       
072900     MOVE KOLLI-ADRUTNIV            TO LISTA-ADRUTNIV                     
073000     MOVE KOLLI-ADVMODUL            TO RESP-L128-ADVMODUL                 
073100     MOVE KOLLI-ADHMODUL            TO RESP-L128-ADHMODUL                 
073200     MOVE KOLLI-IDKOLLI-FLER        TO WS-IDKOLLI-FLER                    
073300     MOVE KOLLI-VKORDBTO-KOLLI      TO WS-VKORDBTO                        
073400     MOVE KOLLI-KDORDKL             TO WS-KDORDKL                         
073500     MOVE KOLLI-KDKOLLI             TO WS-KOLLI-KDKOLLI                   
073600     MOVE KOLLI-IDTRPTNR            TO WS-KOLLI-IDTRPTNR                  
073700     .                                                                    
073800     EJECT                                                                
073900 D-PRINT-LIST              SECTION.                                       
074000                                                                          
074100     IF REQU-L128-IDKOLLI-TOM (WS-CNT)= 00000                             
074200        MOVE WS-CNT        TO RESP-REP-KVRADER                            
074300     ELSE                                                                 
074400        MOVE WS-CASE-CNT   TO RESP-REP-KVRADER                            
074500     END-IF                                                               
074600                                                                          
074700     MOVE '001'         TO RESP-REP-IDPTYP   (RESP-REP-KVRADER)           
074800     MOVE REQU-L128-IDDC-KEY (WS-CNT)                                     
074900                        TO RESP-REP-IDDC     (RESP-REP-KVRADER)           
075000     MOVE WS-IDDISTR    TO RESP-REP-IDDISTR  (RESP-REP-KVRADER)           
075100                           WS-BARCODE-DISTR                               
075200     MOVE WS-IDKUNDNR   TO RESP-REP-IDKUNDNR (RESP-REP-KVRADER)           
075300                           WS-BARCODE-KUNDNR                              
075400                                                                          
075500*TACDIS INFO START                                                        
075600     MOVE NEJ        TO RESP-REP-FLTACDISKND (RESP-REP-KVRADER)           
075700                                                                          
075800     IF (OHUV-IDSYSTEM = 'LDC ' OR 'TACD')                                
075900     AND (DIST13-SVERIGE OR DIST05-NORGE)                                 
076000     AND GMT-FLLDCKND = JA                                                
076100                                                                          
076200       MOVE OHUV-IDGMTREF       TO W-IDGMTREF                             
076300       PERFORM IMS-GU-WDI201                                              
076400                                                                          
076500       IF SEGMENT-FINNS                                                   
076600*                                                                         
076700*UPPGIFTER FÖR NEDRE DELEN AV TACDIS KOLLIFLAGGA                          
076800*                                                                         
076900         MOVE JA     TO RESP-REP-FLTACDISKND (RESP-REP-KVRADER)           
077000*                                                                         
077100         MOVE TAKF-BEGMT-RAD1   TO WS-TAKF-BEGMT-RAD1                     
077200         MOVE TAKF-BEGMT-RAD2   TO WS-TAKF-BEGMT-RAD2                     
077300*                                                                         
077400         MOVE TAKF-BETELNR-TACD                                           
077500           TO RESP-REP-BETELNR-TACD(RESP-REP-KVRADER)                     
077600         MOVE TAKF-IDBILREG                                               
077700           TO RESP-REP-IDBILREG(RESP-REP-KVRADER)                         
077800         MOVE TAKF-TETACDBO                                               
077900           TO RESP-REP-TETACDBO(RESP-REP-KVRADER)                         
078000         MOVE TAKF-BEMEKAN                                                
078100           TO RESP-REP-BEMEKAN(RESP-REP-KVRADER)                          
078200         MOVE TAKF-FLFPLOCK                                               
078300           TO RESP-REP-FLFPLOCK(RESP-REP-KVRADER)                         
078400         MOVE TAKF-TIHHMM       TO WS-TID                                 
078500         MOVE WS-TID (1:2)                                                
078600           TO RESP-REP-HOUR-TACDIS (RESP-REP-KVRADER)                     
078700         MOVE WS-TID (3:2)                                                
078800           TO RESP-REP-MINUTE-TACDIS (RESP-REP-KVRADER)                   
078900       ELSE                                                               
079000         MOVE ALL X'20'                                                   
079100           TO RESP-REP-BETELNR-TACD(RESP-REP-KVRADER)                     
079200         MOVE ALL X'20'                                                   
079300           TO RESP-REP-IDBILREG(RESP-REP-KVRADER)                         
079400         MOVE ALL X'20'                                                   
079500           TO RESP-REP-TETACDBO(RESP-REP-KVRADER)                         
079600         MOVE ALL X'20'                                                   
079700           TO RESP-REP-BEMEKAN(RESP-REP-KVRADER)                          
079800         MOVE ALL X'20'                                                   
079900           TO RESP-REP-FLFPLOCK(RESP-REP-KVRADER)                         
080000         MOVE ALL X'20'                                                   
080100           TO RESP-REP-HOUR-TACDIS (RESP-REP-KVRADER)                     
080200         MOVE ALL X'20'                                                   
080300           TO RESP-REP-MINUTE-TACDIS (RESP-REP-KVRADER)                   
080400       END-IF                                                             
080500*                                                                         
080600       IF ((OHUV-IDSYSTEM = 'LDC' OR 'TACD')                              
080700          AND (DIST13-SVERIGE OR DIST05-NORGE)                            
080800          AND OHUV-IDGROSS NUMERIC                                        
080900          AND OHUV-IDGROSS > ZERO )                                       
081000          MOVE WS-IDKUNDNR (4:3)                                          
081100            TO RESP-REP-IDKUNDNR (RESP-REP-KVRADER) (1:3)                 
081200          MOVE OHUV-IDGROSS                                               
081300            TO RESP-REP-IDKUNDNR (RESP-REP-KVRADER) (4:3)                 
081400*GROSSIST ADRESS                                                          
081500*UPPGIFTER FÖR ÖVRE  DELEN AV TACDIS KOLLIFLAGGA                          
081600         IF OHUV-BEGMT-RAD1 > SPACE                                       
081700           MOVE OHUV-BEGMT-RAD1   TO RESP-REP-BEGMT-RAD1                  
081800                                           (RESP-REP-KVRADER)             
081900         ELSE                                                             
082000           MOVE GMT-BEGMT-RAD1 TO RESP-REP-BEGMT-RAD1                     
082100                                   (RESP-REP-KVRADER)                     
082200         END-IF                                                           
082300         IF OHUV-BEGMT-RAD2 > SPACE                                       
082400           MOVE OHUV-BEGMT-RAD2   TO RESP-REP-BEGMT-RAD2                  
082500                                     (RESP-REP-KVRADER)                   
082600         ELSE                                                             
082700           MOVE GMT-BEGMT-RAD2 TO RESP-REP-BEGMT-RAD2                     
082800                                  (RESP-REP-KVRADER)                      
082900         END-IF                                                           
083000         IF OHUV-ADGMT-GATA > SPACE                                       
083100           MOVE OHUV-ADGMT-GATA   TO RESP-REP-ADGMT-GATA                  
083200                                         (RESP-REP-KVRADER)               
083300         ELSE                                                             
083400           MOVE GMT-ADGMT-GATA TO RESP-REP-ADGMT-GATA                     
083500                                 (RESP-REP-KVRADER)                       
083600         END-IF                                                           
083700         IF OHUV-ADGMT-PADR > SPACE                                       
083800           MOVE OHUV-ADGMT-PADR   TO RESP-REP-ADGMT-PADR                  
083900                                    (RESP-REP-KVRADER)                    
084000         ELSE                                                             
084100           MOVE GMT-ADGMT-PADR TO RESP-REP-ADGMT-PADR                     
084200                                   (RESP-REP-KVRADER)                     
084300         END-IF                                                           
084400       ELSE                                                               
084500*UPPGIFTER FÖR ÖVRE  DELEN AV TACDIS KOLLIFLAGGA                          
084600*KUND REG ADRESS                                                          
084700         MOVE GMT-BEGMT-RAD1 TO RESP-REP-BEGMT-RAD1                       
084800                                 (RESP-REP-KVRADER)                       
084900*                                                                         
085000         MOVE GMT-BEGMT-RAD2 TO RESP-REP-BEGMT-RAD2                       
085100                                 (RESP-REP-KVRADER)                       
085200*                                                                         
085300         MOVE GMT-ADGMT-GATA TO RESP-REP-ADGMT-GATA                       
085400                                 (RESP-REP-KVRADER)                       
085500*                                                                         
085600         MOVE GMT-ADGMT-PADR TO RESP-REP-ADGMT-PADR                       
085700                                 (RESP-REP-KVRADER)                       
085800       END-IF                                                             
085900*                                                                         
086000*UPPGIFTER FÖR NEDRE DELEN AV TACDIS KOLLIFLAGGA                          
086100       MOVE '278'            TO TRAUTF8-KDCP                              
086200       MOVE 35               TO TRAUTF8-KVMAXTL                           
086300*                                                                         
086400       IF WS-TAKF-BEGMT-RAD1 = SPACE                                      
086500                                                                          
086600         IF OHUV-BEGMT-RAD1 = SPACE                                       
086700           MOVE GMT-BEGMT-RAD1                                            
086800             TO RESP-REP-KUNDINFO-RAD1(RESP-REP-KVRADER)                  
086900         ELSE                                                             
087000           MOVE OHUV-BEGMT-RAD1                                           
087100             TO RESP-REP-KUNDINFO-RAD1 (RESP-REP-KVRADER)                 
087200         END-IF                                                           
087300*                                                                         
087400       ELSE                                                               
087500         MOVE WS-TAKF-BEGMT-RAD1                                          
087600           TO RESP-REP-KUNDINFO-RAD1(RESP-REP-KVRADER)                    
087700       END-IF                                                             
087800                                                                          
087900       IF WS-TAKF-BEGMT-RAD2 = SPACE                                      
088000                                                                          
088100         IF OHUV-BEGMT-RAD2 = SPACE                                       
088200           MOVE GMT-BEGMT-RAD2                                            
088300             TO RESP-REP-KUNDINFO-RAD2 (RESP-REP-KVRADER)                 
088400         ELSE                                                             
088500           MOVE OHUV-BEGMT-RAD2                                           
088600             TO RESP-REP-KUNDINFO-RAD2 (RESP-REP-KVRADER)                 
088700         END-IF                                                           
088800       ELSE                                                               
088900         MOVE WS-TAKF-BEGMT-RAD2                                          
089000           TO RESP-REP-KUNDINFO-RAD2(RESP-REP-KVRADER)                    
089100       END-IF                                                             
089200*                                                                         
089300       MOVE RESP-REP-KUNDINFO-RAD1(RESP-REP-KVRADER)                      
089400                                TO TRAUTF8-TECONV-FROM                    
089500       CALL WTRAUTF8    USING TRAUTF8-AREA                                
089600       MOVE TRAUTF8-TECONV-TO                                             
089700         TO RESP-REP-KUNDINFO-RAD1(RESP-REP-KVRADER)                      
089800*                                                                         
089900       MOVE RESP-REP-KUNDINFO-RAD2 (RESP-REP-KVRADER)                     
090000                                TO TRAUTF8-TECONV-FROM                    
090100       CALL WTRAUTF8    USING TRAUTF8-AREA                                
090200       MOVE TRAUTF8-TECONV-TO                                             
090300         TO RESP-REP-KUNDINFO-RAD2 (RESP-REP-KVRADER)                     
090400                                                                          
090500     INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                           
090600             REPLACING ALL '¤' BY 'o'                                     
090700     INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                           
090800             replacing all 'é' by 'e'                                     
090900     INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                           
091000             REPLACING ALL 'É' BY 'E'                                     
091100     INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                           
091200             REPLACING ALL 'ü' BY 'u'                                     
091300     INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                           
091400             REPLACING ALL 'Ü' BY 'U'                                     
091500                                                                          
091600       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
091700               replacing all 'å' by 'a'                                   
091800       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
091900               REPLACING ALL 'Å' BY 'A'                                   
092000       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
092100               REPLACING ALL 'ä' BY 'a'                                   
092200       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
092300               REPLACING ALL 'Ä' BY 'A'                                   
092400       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
092500               REPLACING ALL 'ö' BY 'o'                                   
092600       INSPECT RESP-REP-BEMEKAN(RESP-REP-KVRADER)                         
092700               REPLACING ALL 'Ö' BY 'O'                                   
092800                                                                          
092900*TACDIS INFO END                                                          
093000     END-IF                                                               
093100                                                                          
093200*IDBILREG                                                                 
093300     MOVE OHUV-IDBILREG TO RESP-REP-IDBILREG (RESP-REP-KVRADER)           
093400                                                                          
093500     MOVE OHUV-TIREPDAT   TO RESP-REP-TIREPDAT (RESP-REP-KVRADER)         
093600                                                                          
093700     MOVE WS-IDDISTR               TO TEST-IDDISTR                        
093800                                                                          
093900*BEKUNDRF                                                                 
094000     IF LDC-FR-3P OR SDC-NL OR LDC-GB-3A OR LDC-GB-3B                     
094100       IF DIST30-FRANCE OR LDC-GB-3A OR LDC-GB-3B                         
094200         MOVE OHUV-BEKUNDRF                                               
094300           TO RESP-REP-BEKUNDRF(RESP-REP-KVRADER)                         
094400       ELSE                                                               
094500         MOVE SPACE     TO RESP-REP-BEKUNDRF (RESP-REP-KVRADER)           
094600       END-IF                                                             
094700     END-IF                                                               
094800                                                                          
094900     MOVE WS-IDORDNR    TO RESP-REP-IDORDNR  (RESP-REP-KVRADER)           
095000                           WS-BARCODE-ORDNR                               
095100                                                                          
095200     IF WS-IDKOLLI-FLER < ZERO                                            
095300       MOVE ZERO        TO RESP-REP-IDKOLLI  (RESP-REP-KVRADER)           
095400                           WS-BARCODE-KOLLI                               
095500     ELSE                                                                 
095600       MOVE WS-IDKOLLI-PRT TO RESP-REP-IDKOLLI (RESP-REP-KVRADER)         
095700                              WS-BARCODE-KOLLI                            
095800     END-IF                                                               
095900                                                                          
096000     MOVE WS-KDORDKL    TO RESP-REP-KDORDKL  (RESP-REP-KVRADER)           
096100                                                                          
096200     PERFORM S05-GET-INFO-FR-WDE420                                       
096310                                                                          
096400     IF SDC-NL                                                            
096500       MOVE W-IDARTNR     TO RESP-REP-IDARTNR (RESP-REP-KVRADER)          
096600     END-IF                                                               
096700                                                                          
096800     IF INDATA-OK                                                         
096900     MOVE WS-ORAD-BERADREF   TO RESP-REP-BERADREF                         
097000                                (RESP-REP-KVRADER)                        
097100                                                                          
097200*    MOVE 'TESTRADREF'       TO RESP-REP-BERADREF                         
097300*                               (RESP-REP-KVRADER)                        
097400                                                                          
097500*    -- FIX BAD DATA IN GMT-OVR FIELDS                                    
097600     IF GMT-BEGMT-OVR-RAD1 = LOW-VALUE                                    
097700        MOVE SPACE TO GMT-BEGMT-OVR-RAD1                                  
097800     END-IF                                                               
097900     IF GMT-BEGMT-OVR-RAD2 = LOW-VALUE                                    
098000        MOVE SPACE TO GMT-BEGMT-OVR-RAD2                                  
098100     END-IF                                                               
098200     IF GMT-ADGMT-OVR-GATA = LOW-VALUE                                    
098300        MOVE SPACE TO GMT-ADGMT-OVR-GATA                                  
098400     END-IF                                                               
098500     IF GMT-ADGMT-OVR-PADR = LOW-VALUE                                    
098600        MOVE SPACE TO GMT-ADGMT-OVR-PADR                                  
098700     END-IF                                                               
098800     IF GMT-ADGMT-OVR-LAND = LOW-VALUE                                    
098900        MOVE SPACE TO GMT-ADGMT-OVR-LAND                                  
099000     END-IF                                                               
099100     IF GMT-IDTFN          = LOW-VALUE                                    
099200        MOVE SPACE TO GMT-IDTFN                                           
099300     END-IF                                                               
099400                                                                          
099500     IF (NDC-CN OR LDC-CN OR NDC-JP)                                      
099600     AND (                                                                
099700*        -- IF OVR FIELDS CONTAIN ANY SIGNIFICANT VALUES --               
099800             GMT-BEGMT-OVR-RAD1 NOT = SPACE                               
099900          OR GMT-BEGMT-OVR-RAD2 NOT = SPACE                               
100000          OR GMT-ADGMT-OVR-GATA NOT = SPACE                               
100100          OR GMT-ADGMT-OVR-PADR NOT = SPACE                               
100200         )                                                                
100300*      -- ADDRESS IN DOUBLE-BYTE CHINESE CODE FETCHED FROM                
100400*      -- CUSTOMER DATABASE "OVR" FIELDS INSTEAD OF ORDER HEAD            
100500       IF NDC-JP                                                          
100600         MOVE '930'          TO TRAUTF8-KDCP                              
100700       ELSE                                                               
100800         MOVE '935'          TO TRAUTF8-KDCP                              
100900       END-IF                                                             
101000                                                                          
101100       MOVE GMT-BEGMT-OVR-RAD1    TO RESP-REP-BEGMT-RAD1                  
101200                                       (RESP-REP-KVRADER)                 
101300       MOVE GMT-BEGMT-OVR-RAD2    TO RESP-REP-BEGMT-RAD2                  
101400                                       (RESP-REP-KVRADER)                 
101500       MOVE GMT-ADGMT-OVR-GATA    TO RESP-REP-ADGMT-GATA                  
101600                                       (RESP-REP-KVRADER)                 
101700       MOVE GMT-ADGMT-OVR-PADR    TO RESP-REP-ADGMT-PADR                  
101800                                       (RESP-REP-KVRADER)                 
101900*ADRESS-5                                                                 
102000       MOVE ALL   X'20'           TO RESP-REP-ADGMT-LAND                  
102100                                       (RESP-REP-KVRADER)                 
102200     ELSE                                                                 
102300       IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                               
102400       AND (DIST13-SVERIGE OR DIST05-NORGE)                               
102500       AND GMT-FLLDCKND = JA                                              
102600         CONTINUE                                                         
102700*FÖR TACDIS HAR DATA REDAN FLYTTATS FRÅN KUNDREG (GMT)                    
102800       ELSE                                                               
102900*STD-KOLLIFLAGGA                                                          
103000*        -- ADDRESS IN NORMAL EBCDIC CODE FROM ORDER HEAD                 
103100         MOVE '278'          TO TRAUTF8-KDCP                              
103200                                                                          
103300         MOVE OHUV-BEGMT-RAD1     TO RESP-REP-BEGMT-RAD1                  
103400                                         (RESP-REP-KVRADER)               
103500         MOVE OHUV-BEGMT-RAD2     TO RESP-REP-BEGMT-RAD2                  
103600                                         (RESP-REP-KVRADER)               
103700         MOVE OHUV-ADGMT-GATA     TO RESP-REP-ADGMT-GATA                  
103800                                         (RESP-REP-KVRADER)               
103900         MOVE OHUV-ADGMT-PADR     TO RESP-REP-ADGMT-PADR                  
104000                                         (RESP-REP-KVRADER)               
104100*ADRESS-5                                                                 
104200         MOVE OHUV-ADGMT-LAND     TO RESP-REP-ADGMT-LAND                  
104300                                         (RESP-REP-KVRADER)               
104400       END-IF                                                             
104500     END-IF                                                               
104600                                                                          
104700     MOVE 35 TO TRAUTF8-KVMAXTL                                           
104800                                                                          
104900     IF  (RESP-REP-BEGMT-RAD1 (RESP-REP-KVRADER)                          
105000                                            = SPACE OR LOW-VALUE)         
105100     AND (RESP-REP-BEGMT-RAD2 (RESP-REP-KVRADER)                          
105200                                            = SPACE OR LOW-VALUE)         
105300*      -- SHIFT UP TWO LINES                                              
105400       MOVE RESP-REP-ADGMT-GATA (RESP-REP-KVRADER)                        
105500                                TO TRAUTF8-TECONV-FROM                    
105600       CALL WTRAUTF8    USING TRAUTF8-AREA                                
105700       MOVE TRAUTF8-TECONV-TO TO RESP-REP-BEGMT-RAD1                      
105800                                       (RESP-REP-KVRADER)                 
105900                                                                          
106000       MOVE RESP-REP-ADGMT-PADR(RESP-REP-KVRADER)                         
106100                                TO TRAUTF8-TECONV-FROM                    
106200       CALL WTRAUTF8    USING TRAUTF8-AREA                                
106300       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-BEGMT-RAD2                    
106400                                       (RESP-REP-KVRADER)                 
106500                                                                          
106600       MOVE ALL   X'20'         TO RESP-REP-ADGMT-GATA                    
106700                                       (RESP-REP-KVRADER)                 
106800                                   RESP-REP-ADGMT-PADR                    
106900                                       (RESP-REP-KVRADER)                 
107000     ELSE                                                                 
107100       MOVE RESP-REP-BEGMT-RAD1(RESP-REP-KVRADER)                         
107200                                TO TRAUTF8-TECONV-FROM                    
107300       CALL WTRAUTF8    USING TRAUTF8-AREA                                
107400       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-BEGMT-RAD1                    
107500                                       (RESP-REP-KVRADER)                 
107600                                                                          
107700       MOVE RESP-REP-BEGMT-RAD2(RESP-REP-KVRADER)                         
107800                                TO TRAUTF8-TECONV-FROM                    
107900       CALL WTRAUTF8    USING TRAUTF8-AREA                                
108000       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-BEGMT-RAD2                    
108100                                       (RESP-REP-KVRADER)                 
108200                                                                          
108300       MOVE RESP-REP-ADGMT-GATA(RESP-REP-KVRADER)                         
108400                                TO TRAUTF8-TECONV-FROM                    
108500       CALL WTRAUTF8    USING TRAUTF8-AREA                                
108600       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-ADGMT-GATA                    
108700                                       (RESP-REP-KVRADER)                 
108800                                                                          
108900       MOVE RESP-REP-ADGMT-PADR(RESP-REP-KVRADER)                         
109000                                TO TRAUTF8-TECONV-FROM                    
109100       CALL WTRAUTF8    USING TRAUTF8-AREA                                
109200       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-ADGMT-PADR                    
109300                                       (RESP-REP-KVRADER)                 
109400                                                                          
109500*ADRESS-5   ADGMT-LAND                                                    
109600       MOVE RESP-REP-ADGMT-LAND(RESP-REP-KVRADER)                         
109700                                TO TRAUTF8-TECONV-FROM                    
109800       CALL WTRAUTF8    USING TRAUTF8-AREA                                
109900       MOVE TRAUTF8-TECONV-TO   TO RESP-REP-ADGMT-LAND                    
110000                                       (RESP-REP-KVRADER)                 
110100                                                                          
110200     END-IF                                                               
110300                                                                          
110400     MOVE LISTA-KDFRAKT      TO RESP-REP-KDFRAKT IN RESP-REP-RAD          
110500                                        (RESP-REP-KVRADER)                
110600     MOVE LISTA-ADRUTNIV     TO RESP-REP-ADRUTNIV                         
110700                               IN RESP-REP-RAD (RESP-REP-KVRADER)         
110800     MOVE LISTA-TIRFS        TO RESP-REP-TIRFSDAT                         
110900                                (RESP-REP-KVRADER)                        
110910     MOVE ODEL-IDLOPNR-ORD     TO RESP-REP-IDLOPNR-ORD                    
110920                                  (RESP-REP-KVRADER)                      
111000                                                                          
111100     MOVE WS-KOLLI-ADFLGEO   TO RESP-REP-ADFLGEO IN RESP-REP-RAD          
111200                                   (RESP-REP-KVRADER)                     
111300     MOVE WS-KOLLI-ADFLOMR   TO RESP-REP-ADFLOMR IN RESP-REP-RAD          
111400                                      (RESP-REP-KVRADER)                  
111500     MOVE WS-OHUV-IDDEPT     TO RESP-REP-IDDEPT (RESP-REP-KVRADER)        
111600                                                                          
111700     MOVE WS-KOLLI-KDKOLLI   TO RESP-REP-KDKOLLI(RESP-REP-KVRADER)        
111800                                                                          
111900     MOVE WS-KOLLI-IDTRPTNR  TO                                           
112000                             RESP-REP-IDTRPTNR(RESP-REP-KVRADER)          
112100                                                                          
112200     MOVE WS-VKORDBTO   TO RESP-REP-VKORDBTO (RESP-REP-KVRADER)           
112300                                                                          
112400     MOVE WS-BARCODE    TO RESP-REP-BARCODE  (RESP-REP-KVRADER)           
112500     ELSE                                                                 
112600                                                                          
112700*IDBILREG                                                                 
112800     MOVE OHUV-IDBILREG TO RESP-REP-IDBILREG (RESP-REP-KVRADER)           
112900                                                                          
113000     MOVE OHUV-TIREPDAT   TO RESP-REP-TIREPDAT (RESP-REP-KVRADER)         
113100     END-IF                                                               
113200                                                                          
113300     .                                                                    
113400     EJECT                                                                
113500 F-HAEMTA-ADRESS           SECTION.                                       
113600                                                                          
113700     PERFORM IMS-GET-WLORQI01                                             
113800     PERFORM S06-DIST-KUND-LDC                                            
113900     MOVE OHUV-IDDISTR    TO DIST03-IDDISTR                               
114000                                                                          
114100*    IF GMT-FLLDCKND = JA AND KDFRAKT-FINNS                               
114200*ISSUE ID : 8688683                                                       
114300     IF GMT-FLLDCKND = JA                                                 
114400       IF OHUV-IDDEPT > ZERO                                              
114500         MOVE OHUV-IDDEPT TO WS-OHUV-IDDEPT                               
114600       END-IF                                                             
114700     END-IF                                                               
114800                                                                          
114900     PERFORM IMS-GNP-WLORQI12                                             
115000                                                                          
115100     IF SEGMENT-FINNS                                                     
115200        IF ARB-BEGMRK             = 'SPECIALMÄRKNING'                     
115300           CONTINUE                                                       
115400        ELSE                                                              
115500           IF ARB-BEGMRK            = SPACE                               
115600             MOVE REQU-L128-IDDC-KEY (WS-CNT)                             
115700                                  TO W-501-IDDC                           
115800                                     W-501-IDDC-DEFAULT                   
115900             MOVE WS-IDDISTR      TO W-501-IDDISTR                        
116000                                     W-501-IDDISTR-DEFAULT                
116100             MOVE WS-IDKUNDNR     TO W-501-IDKUNDNR                       
116200             PERFORM IMS-GU-GMTC01-WDB501                                 
116300             IF SEGMENT-FINNS                                             
116400                CONTINUE                                                  
116500             ELSE                                                         
116600                MOVE FEL-X-NOT-FOUND TO RESP-IDMSG-ERROR                  
116700                MOVE FEL-KDFRAKT     TO RESP-IDELMT-ERROR                 
116800             END-IF                                                       
116900           ELSE                                                           
117000             MOVE WS-IDKUNDNR TO W-IDKUNDNR                               
117100             MOVE WS-IDDISTR  TO W-IDDISTR                                
117200             PERFORM IMS-GU-GMTA01-WDB201                                 
117300           END-IF                                                         
117400        END-IF                                                            
117500     ELSE                                                                 
117600        MOVE 'IDORDNR'          TO RESP-IDELMT-ERROR                      
117700        MOVE '041'              TO RESP-IDMSG-ERROR                       
117800     END-IF                                                               
117900     .                                                                    
118000     SKIP2                                                                
118800 S05-GET-INFO-FR-WDE420   SECTION.                                        
118900     MOVE 'STA S05- SEC  '    TO FELTEXT                                  
119000                                                                          
119100     PERFORM  IMS-GU-WDE601                                               
119200     IF SEGMENT-FINNS                                                     
119300       MOVE VORD-IDPRODNR     TO W-IDPRODNR-WDE421                        
119400       MOVE KOLLI-IDKOLLI     TO W-IDKOLLI-WDE421                         
119500       PERFORM IMS-GU-WDE411-01                                           
119600       IF SEGMENT-FINNS                                                   
119700         MOVE ORAD-IDARTNR    TO W-IDARTNR                                
119800         MOVE FSEQ-KORD-IDORDER  TO W-301-IDORDER                         
119810         MOVE FSEQ-KORD-IDDC     TO W-301-IDDC                            
119820         MOVE FSEQ-KORD-IDPRODNR TO W-301-IDPRODNR                        
119830         MOVE FSEQ-KORD-IDPLKLST TO W-301-IDPLKLST                        
119840                                                                          
119841                                                                          
119850         PERFORM IMS-GU-WLORQA01                                          
119860         MOVE ODEL-DARFS      TO WS-DARFS                                 
119870         MOVE WS-DARFS-YYMMDD TO LISTA-TIRFS                              
119880                                                                          
119900         IF GMT-FLLDCKND = JA                                             
120000           IF ORAD-IDKUNDRF-WIP > SPACE                                   
120100             MOVE ORAD-IDKUNDRF-WIP TO WS-ORAD-BERADREF                   
120200           ELSE                                                           
120300             MOVE ORAD-BERADREF    TO WS-ORAD-BERADREF                    
120400           END-IF                                                         
120500* FÖR TACDIS ORDER MÖRKAS JOBB NR. I BERADREF                             
120600                                                                          
120700           MOVE WS-IDDISTR         TO TEST-IDDISTR                        
120800           IF (OHUV-IDSYSTEM = 'LDC' OR 'TACD')                           
120900           AND (DIST13-SVERIGE OR DIST05-NORGE)                           
121000             MOVE SPACE            TO WS-ORAD-BERADREF(9:2)               
121100           END-IF                                                         
121200         ELSE                                                             
121300           MOVE ORAD-BERADREF       TO WS-ORAD-BERADREF                   
121400         END-IF                                                           
121500                                                                          
121600         MOVE WS-IDDISTR            TO TEST-IDDISTR                       
121700         IF DIST85-ITALIEN                                                
121800           IF OHUV-BEKUNDRF > SPACE                                       
121900             MOVE OHUV-BEKUNDRF     TO WS-ORAD-BERADREF                   
122000           ELSE                                                           
122100             IF ORAD-BERADREF > SPACE                                     
122200               MOVE ORAD-BERADREF   TO WS-ORAD-BERADREF                   
122300             ELSE                                                         
122400               IF ORAD-BEVOLREF > SPACE                                   
122500                 MOVE ORAD-BEVOLREF TO WS-ORAD-BERADREF                   
122600               END-IF                                                     
122700             END-IF                                                       
122800           END-IF                                                         
122900         END-IF                                                           
123000                                                                          
123100*RENSNIG I JSP SIDAN BEHOVS AV VILLKOR FOR REP-RESTORDER                  
123200         MOVE ALL '+'            TO RESP-REP-RESTORDER                    
123300                                   (RESP-REP-KVRADER)                     
123400       ELSE                                                               
123500         MOVE NEJ                TO WS-INDATA-SW                          
123600         MOVE FEL-LINE-NOT-FOUND TO RESP-IDMSG-ERROR                      
123700         MOVE FEL-IDKOLLI        TO RESP-IDELMT-ERROR                     
123800       END-IF                                                             
123900     END-IF                                                               
124000     .                                                                    
124100     SKIP2                                                                
124200 S06-DIST-KUND-LDC SECTION.                                               
124300                                                                          
124400     MOVE OHUV-IDDISTR        TO  W-IDDISTR                               
124500     MOVE OHUV-IDKUNDNR       TO  W-IDKUNDNR                              
124600                                                                          
124700     PERFORM IMS-GU-GMTA01-WDB201                                         
124800                                                                          
124900     IF SEGMENT-SAKNAS                                                    
125000        MOVE NEJ              TO GMT-FLLDCKND                             
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400                                                                          
125500* DISPATCHER-SEKTIONER                                                    
125600     SKIP3                                                                
125700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
125800                                                                          
125900     MOVE 'GETARG'                        TO SUB-KDFUNC                   
126000     MOVE 'CARPARTS.LDC.PRCASELABEL'  TO SUB-ADDISPABS                    
126100     MOVE LENGTH OF REQU-AREA             TO SUB-KVDLEN                   
126200                                                                          
126300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
126400                                                                          
126500     IF SUB-KDRC > 0                                                      
126600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
126700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
126800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
126900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
127000     END-IF                                                               
127100     .                                                                    
127200     SKIP3                                                                
127300 S02-RETURN-DATA     SECTION.                                             
127400                                                                          
127500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
127600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
127700                                                                          
127800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN                       
127900                        RESP-AREA                                         
128000                                                                          
128100     IF SUB-KDRC > 0                                                      
128200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
128300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
128400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
128500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
128600     END-IF                                                               
128700     .                                                                    
128800     SKIP3                                                                
128900* IMS SEKTIONER                                                           
129000* IMS SEKTIONER                                                           
129100* IMS SEKTIONER                                                           
129200     EJECT                                                                
129300 IMS-GU-GMTA01-WDB201      SECTION.                                       
129400     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
129500            DELIMITED BY SIZE INTO SSA1                                   
129600     MOVE '    ' TO GODK-STATUSKODER                                      
129700     CALL CBLTDLI USING GU GMTA-PCB GMT-WDB201 SSA1                       
129800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
129900     PERFORM IMS-STATUSKONTROLL                                           
130000     SKIP3                                                                
130100     .                                                                    
130200                                                                          
130300 IMS-GU-WDI201      SECTION.                                              
130400                                                                          
130500     STRING 'WDI201  (IDGMTREF =' W-IDGMTREF-X ')'                        
130600            DELIMITED BY SIZE INTO SSA1                                   
130700     MOVE '  GE' TO GODK-STATUSKODER                                      
130800     CALL CBLTDLI USING GU WDI2-PCB DLI-IO-WDI2 SSA1                      
130900     MOVE WDI2-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     SKIP3                                                                
131200     .                                                                    
131300                                                                          
131400 IMS-GU-GMTC01-WDB501      SECTION.                                       
131500     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
131600                    '!WDB501KY =' W-WDB501KY-DEFAULT-X ')'                
131700            DELIMITED BY SIZE INTO SSA1                                   
131800     MOVE '  GE' TO GODK-STATUSKODER                                      
131900     CALL CBLTDLI USING GU GMTC-PCB FK-WDB501 SSA1                        
132000     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     SKIP3                                                                
132300     .                                                                    
132400 IMS-GU-WDE401-ASEQ        SECTION.                                       
132500                                                                          
132600     STRING 'WDE401  (WDE4ASEQ =' W-KUNDORDER-SEK-X ')'                   
132700                      DELIMITED BY SIZE INTO SSA1                         
132800     MOVE '  GE' TO GODK-STATUSKODER                                      
132900     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
133000     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
133100                               STATUS-KUNDORDER-SEK-WS                    
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400     SKIP3                                                                
133500 IMS-GU-WDE411-01          SECTION.                                       
133600                                                                          
133700     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE421KY-X ')'                      
133800                      DELIMITED BY SIZE INTO SSA1                         
133810     move 'WDE401' TO SSA2                                                
133900     MOVE '  GE' TO GODK-STATUSKODER                                      
134000     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411-01 SSA1 SSA2            
134100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
134200                               STATUS-KUNDORDER-SEK-WS                    
134300     PERFORM IMS-STATUSKONTROLL                                           
134400     .                                                                    
134500     SKIP3                                                                
134600 IMS-GN-WDE401             SECTION.                                       
134700                                                                          
134800     STRING 'WDE401  (WDE4ASEQ =' W-KUNDORDER-SEK-X ')'                   
134900                      DELIMITED BY SIZE INTO SSA1                         
135000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
135100     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
135200     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
135300                               STATUS-KUNDORDER-SEK-WS                    
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     .                                                                    
135600     EJECT                                                                
135700 IMS-GU-WDE401             SECTION.                                       
135800                                                                          
135900     STRING 'WDE401  (WDE401KY =' W-KUNDORDER-X ')'                       
136000                      DELIMITED BY SIZE INTO SSA1                         
136100     MOVE '    ' TO GODK-STATUSKODER                                      
136200     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
136300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136600     SKIP3                                                                
136700 IMS-GU-WDE611 SECTION.                                                   
136800                                                                          
136900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-WDE611-X ')'                 
137000          DELIMITED BY SIZE INTO SSA1                                     
137100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-WDE611-X ')'                  
137200          DELIMITED BY SIZE INTO SSA2                                     
137300     MOVE '  GE' TO GODK-STATUSKODER                                      
137400     CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE611 SSA1 SSA2           
137500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
137600     PERFORM IMS-STATUSKONTROLL                                           
137700     .                                                                    
137800     EJECT                                                                
137900* LAESN. FÖR KONTROLL OM LAASSEGMENT SKALL TAS BORT                       
138000 IMS-GET-WDE4E SECTION.                                                   
138100     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
138200                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
138300            DELIMITED BY SIZE INTO SSA1                                   
138400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
138500     CALL CBLTDLI USING GN WDE4E-PCB DLI-IOAREA-WDE4E1 SSA1               
138600     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
138700     PERFORM IMS-STATUSKONTROLL                                           
138800     .                                                                    
138900     EJECT                                                                
139000 IMS-GU-WDE601           SECTION.                                         
139100                                                                          
139200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
139300                      DELIMITED BY SIZE INTO SSA1                         
139400     MOVE '  GE' TO GODK-STATUSKODER                                      
139500     CALL CBLTDLI USING GU WDE6-PCB DLI-IOAREA-WDE601 SSA1                
139600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900     SKIP3                                                                
140000 IMS-GET-WLORQI01          SECTION.                                       
140100                                                                          
140200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
140300                      DELIMITED BY SIZE INTO SSA1                         
140400     MOVE '  ' TO GODK-STATUSKODER                                        
140500     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-Q201 SSA1                      
140600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     SKIP3                                                                
141000 IMS-GNP-WLORQI12          SECTION.                                       
141100                                                                          
141200     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
141300                      DELIMITED BY SIZE INTO SSA1                         
141400     MOVE '  GE' TO GODK-STATUSKODER                                      
141500     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-Q212 SSA1                     
141600     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
141700     PERFORM IMS-STATUSKONTROLL                                           
141800     .                                                                    
141900     SKIP2                                                                
142000 IMS-GU-WLORQA01          SECTION.                                        
142100                                                                          
142200     STRING 'WLORQA01(WDQ301KY =' W-WDQ301KY-X ')'                        
142300                      DELIMITED BY SIZE INTO SSA1                         
142400     MOVE '  ' TO GODK-STATUSKODER                                        
142500     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA3 SSA1                     
142600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900     SKIP3                                                                
143000 IMS-GU-ORDD-WDE601 SECTION.                                              
143100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
143200            DELIMITED BY SIZE INTO SSA1                                   
143300     MOVE '  GE' TO GODK-STATUSKODER                                      
143400     CALL CBLTDLI USING GU ORDD-PCB DLI-IOAREA-WDE601 SSA1                
143500     MOVE ORDD-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSKONTROLL                                           
143700     .                                                                    
143800                                                                          
143900 IMS-STATUSKONTROLL        SECTION.                                       
144000                                                                          
144100     SET STATUS-INDX TO 1                                                 
144200     SEARCH GODK-STATUS                                                   
144300       AT END                                                             
144400         CALL FELLOG                                                      
144500       WHEN GODK-STATUS (STATUS-INDX) = STATUS-WS                         
144600         CONTINUE                                                         
144700     END-SEARCH                                                           
145000     .                                                                    
