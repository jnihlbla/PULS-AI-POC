000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034900.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   98/07/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET SKRIVER UT DELIVERY-NOTE TILL NDC-LAGER.              
000900*        PROGRAMMET ANROPAS DÅ EN ORDERDEL ÄR FÄRDIGPACKAD PÅ             
001000*        RESP LAGER                                                       
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDE6                                       
001300*        PROGRAMMET LÄSER      WLORQP (WDQ5)                              
001400*        PROGRAMMET LÄSER      WLORQM (WDQ1)                              
001500*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001600*        PROGRAMMET LÄSER      WDB6                                       
001700*                                                                         
001800*   TILLÄGG JANUARI 2004 AV LINDA NILSSON                                 
001900*        SKICKA DELIVERY-NOTE TILL VCOM                                   
002000*        VIA DISTRIBUTION AND PRINT                                       
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T349                                              
002400*        MID:         W4I34901                                            
002500*                                                                         
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4034900'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 01  FELTEXT.                                                             
003700     03 FILLER                   PIC X(8)  VALUE SPACE.                   
003800     03 FELTEXT-STR              PIC X(72) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  VIPS                        PIC X       VALUE 'V'.                   
004400 77  PULS                        PIC X       VALUE 'P'.                   
004500                                                                          
004600 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT  '.        
004700 01 ARBETSFALT.                                                           
004800                                                                          
004900     03 WS-IDDISTR               PIC S9(05)  VALUE ZERO COMP-3.           
005000     03 WS-IDKUNDNR              PIC S9(07)  VALUE ZERO COMP-3.           
005100     03 WS-IDKUNDNR-DAP          PIC  9(04)  VALUE ZERO.                  
005200     03 WS-IDORDNR               PIC S9(07)  VALUE ZERO COMP-3.           
005300     03 WS-IDORDNR-X             PIC  X(07)  VALUE SPACE.                 
005400     03 WS-IDPRODNR              PIC S9(07)  VALUE ZERO COMP-3.           
005500     03 WS-IDORDER               PIC S9(07)  VALUE ZERO COMP-3.           
005600     03 WS-IDSEKVNR              PIC  9(03)  VALUE ZERO.                  
005700     03 WS-KVBEART               PIC S9(07)  VALUE ZERO COMP-3.           
005800     03 WS-KVBEART-Q             PIC S9(07)  VALUE ZERO COMP-3.           
005900     03 WS-SPAR-IDARTNR          PIC S9(09)  VALUE ZERO COMP-3.           
006000     03 WS-SPAR-IDPURAD          PIC S9(05)  VALUE ZERO COMP-3.           
006100     03 WS-SPAR-IDKOLLI          PIC S9(05)  VALUE ZERO COMP-3.           
006200     03 WS-SPAR-IDLOPNR          PIC S9(03)  VALUE ZERO COMP-3.           
006300     03 WS-SPAR-OBKR-IDLOPNR     PIC S9(03)  VALUE ZERO COMP-3.           
006400     03 WS-BEART-USA             PIC  X(25)  VALUE SPACE.                 
006500     03 WS-KDORDBEK              PIC  X(02)  VALUE SPACE.                 
006600                                                                          
006700     03 WS-IDDC-HOME             PIC  X(02)  VALUE SPACE.                 
006800     03 WS-IDDC-LEV              PIC  X(02)  VALUE SPACE.                 
006900                                                                          
007000     03 WS-SIDRAK                PIC  9(03)  VALUE ZERO.                  
007100     03 WS-SUM-DELIV-LINES       PIC  9(03)  VALUE ZERO.                  
007200     03 WS-RADRAK                PIC  9(03)  VALUE ZERO.                  
007300     03 WS-MAX-RADER             PIC  9(03)  VALUE 55.                    
007400                                                                          
007500     03 WS-VALD-PRINTER          PIC X(08)   VALUE SPACE.                 
007600     03 WS-VKORDBTO              PIC S9(6)V9(01)                          
007700                                             VALUE ZERO COMP-3.           
007800     03 WS-VLORDBTO              PIC S9(6)V9(01)                          
007900                                             VALUE ZERO COMP-3.           
008000                                                                          
008100     03 RED-KDORDBEK             PIC X(03).                               
008200     03 RED-KDORDBEK-DELAR  REDEFINES RED-KDORDBEK.                       
008300        05 RED-KDORDBEK-KOD      PIC X(02).                               
008400        05 RED-KDORDBEK-X        PIC X(01).                               
008500                                                                          
008600 01  FILLER                      PIC X(16)   VALUE 'TIDER       '.        
008700 01 TIDER.                                                                
008800     03 WS-TIREGDAT              PIC 9(06).                               
008900     03 WS-TIREGDAT-YYMMDD  REDEFINES WS-TIREGDAT.                        
009000        05 WS-TIREGDAT-YY        PIC 9(02).                               
009100        05 WS-TIREGDAT-MM        PIC 9(02).                               
009200        05 WS-TIREGDAT-DD        PIC 9(02).                               
009300                                                                          
009400     03 WS-DAREGDAT.                                                      
009500        05 WS-DAREGDAT-SS        PIC 9(02).                               
009600        05 WS-DAREGDAT-YYMMDD    PIC 9(06).                               
009700                                                                          
009800     03 WS-TIREGTID              PIC 9(06).                               
009900     03 WS-TIREGTID-HHMMSS  REDEFINES WS-TIREGTID.                        
010000        05 WS-TIREGTID-HH        PIC 9(02).                               
010100        05 WS-TIREGTID-MM        PIC 9(02).                               
010200        05 WS-TIREGTID-SS        PIC 9(02).                               
010300                                                                          
010400     03 WS-TIUTSKR               PIC 9(06).                               
010500     03 WS-TIUTSKR-YYMMDD   REDEFINES WS-TIUTSKR.                         
010600        05 WS-TIUTSKR-YY         PIC 9(02).                               
010700        05 WS-TIUTSKR-MM         PIC 9(02).                               
010800        05 WS-TIUTSKR-DD         PIC 9(02).                               
010900                                                                          
011000     03 WS-DAUTSKR.                                                       
011100        05 WS-DAUTSKR-SS         PIC 9(02).                               
011200        05 WS-DAUTSKR-YYMMDD     PIC 9(06).                               
011300     03 WS-TIUTSTID              PIC 9(06).                               
011400     03 WS-TIUTSTID-HHMMSS  REDEFINES WS-TIUTSTID.                        
011500        05 WS-TIUTSTID-HH        PIC 9(02).                               
011600        05 WS-TIUTSTID-MM        PIC 9(02).                               
011700        05 WS-TIUTSTID-SS        PIC 9(02).                               
011800                                                                          
011900     03  WS-DN-DETALJRAD-LAST.                                            
012000         05  FILLER                     PIC X(01)  VALUE SPACE.           
012100         05  WS-DN-RAD-IDARTNR-LAST     PIC Z(09).                        
012200         05  WS-DN-RAD-STRECK-LAST      PIC X(01)  VALUE SPACE.           
012300         05  WS-DN-RAD-REKSIFFR-LAST    PIC X(01).                        
012400         05  FILLER                     PIC X(01)  VALUE SPACE.           
012500         05  WS-DN-RAD-BEART-USA-LAST   PIC X(11)  VALUE SPACE.           
012600         05  WS-DN-RAD-KVLEVART-LAST    PIC Z(07).                        
012700         05  FILLER                     PIC X(01)  VALUE SPACE.           
012800         05  WS-DN-RAD-KVBEART-LAST     PIC Z(07).                        
012900         05  FILLER                     PIC X(01)  VALUE SPACE.           
013000         05  WS-DN-RAD-KVRO-LAST        PIC Z(07).                        
013100         05  FILLER                     PIC X(03)  VALUE SPACE.           
013200         05  WS-DN-RAD-KDORDBEK-LAST    PIC X(03).                        
013300         05  FILLER                     PIC X(02)  VALUE SPACE.           
013400         05  WS-DN-RAD-IDDC-LAST        PIC X(02).                        
013500         05  FILLER                     PIC X(01)  VALUE SPACE.           
013600         05  WS-DN-RAD-IDKOLLI-LAST     PIC Z(05).                        
013700         05  FILLER                     PIC X(01)  VALUE SPACE.           
013800         05  WS-DN-RAD-BERADREF-LAST    PIC X(10).                        
013900         05  FILLER                     PIC X(01)  VALUE SPACE.           
014000         05  WS-DN-RAD-IDKUNDRF-RO-LAST PIC Z(05).                        
014100         05  FILLER                     PIC X(01)  VALUE SPACE.           
014200 01  WS-DUMMY                    PIC X(132) VALUE SPACE.                  
014300                                                                          
014400*    --- STYRTECKEN PRINTER                                               
014500 01  WS-PAGESKIP                 PIC X      VALUE '1'.                    
014600 01  WS-SKIP1                    PIC X      VALUE ' '.                    
014700 01  WS-SKIP2                    PIC X      VALUE '0'.                    
014800 01  WS-SKIP3                    PIC X      VALUE '-'.                    
014900                                                                          
015000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
015100                                                                          
015200 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR    '.        
015300                                                                          
015400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
015500     88  INDATA-OK                           VALUE 'J'.                   
015600     88  INDATA-FEL                          VALUE 'N'.                   
015700                                                                          
015800 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
015900     88  TRAFF                               VALUE 'J'.                   
016000                                                                          
016100 77  PULS-ERS-SW                 PIC X       VALUE 'N'.                   
016200     88  PULSERS                             VALUE 'J'.                   
016300                                                                          
016400 77  PULS-OBKR-SW                PIC X       VALUE 'N'.                   
016500     88  PULSOBKR                            VALUE 'J'.                   
016600                                                                          
016700 77  PULSERS-VIPSTILLK-SW        PIC X       VALUE 'N'.                   
016800     88  PULSERS-VIPSTILLK                   VALUE 'J'.                   
016900                                                                          
017000 77  PULSOBKR-VIPSTILLK-SW       PIC X       VALUE 'N'.                   
017100     88  PULSOBKR-VIPSTILLK                  VALUE 'J'.                   
017200                                                                          
017300 77  TILLKOMMANDE-SW             PIC X       VALUE 'N'.                   
017400     88  VIPSTILLKOMMANDE                    VALUE 'V'.                   
017500     88  PULSTILLKOMMANDE                    VALUE 'P'.                   
017600     88  EJ-TILLKOMMANDE                     VALUE 'N'.                   
017700                                                                          
017800 77  SPARRTEXT-SW                PIC X       VALUE 'N'.                   
017900     88  SPARRTEXT                           VALUE 'J'.                   
018000     88  EJ-SPARRTEXT                        VALUE 'N'.                   
018100                                                                          
018200 77  LEVRAD-SW                   PIC X       VALUE 'N'.                   
018300     88  SKRIV-LEVRAD                        VALUE 'J'.                   
018400                                                                          
018500 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
018600     88  FIRST-TIME                          VALUE 'J'.                   
018700     88  NOT-FIRST-TIME                      VALUE 'N'.                   
018800                                                                          
018900 77  ORDERDEL-UTSKRIVEN-SW       PIC X       VALUE 'N'.                   
019000     88  ORDERDEL-UTSKRIVEN                  VALUE 'J'.                   
019100     88  ORDERDEL-EJ-UTSKRIVEN               VALUE 'N'.                   
019200                                                                          
019300 77  PACKNING-STARTAD-SW         PIC X       VALUE 'N'.                   
019400     88  PACKNING-STARTAD                    VALUE 'J'.                   
019500     88  PACKNING-EJ-STARTAD                 VALUE 'N'.                   
019600                                                                          
019700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
019800     88  EGEN-MID                            VALUE '4349'.                
019900     88  GODK-MID                            VALUE '4341'                 
020000                                                   '4397'                 
020100                                                   '434Y'                 
020200                                                   'L197'                 
020300                                                   'L199'.                
020400*      --- VALID IDDC CODES                                               
020500*                                                                         
020600*01    -COPY WWDC99                                                       
020700       EJECT                                                              
020800 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
020900     SKIP3                                                                
021000*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
021100     EJECT                                                                
021200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021300 01  FILLER                      PIC X(16)   VALUE 'SUBPROGRAM  '.        
021400 01  GENERELLA-SUBPROGRAM.                                                
021500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
021900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
022000     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
022100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
022200     EJECT                                                                
022300                                                                          
022400*    --- PARAMETRAR TILL ABEND                                            
022500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022800                                                                          
022900*01  -COPY W006PRAR                                                       
023000     EJECT                                                                
023100                                                                          
023200 01  FILLER                      PIC X(16)   VALUE 'W009KSIF'.            
023300 01  W009KSIF-PARM.                                                       
023400     03 KSIF-FLT                 PIC 9(9).                                
023500     03 KSIF-LGD                 PIC 9(1).                                
023600     03 KSIF-KSIFF               PIC 9(9).                                
023700     SKIP3                                                                
023800                                                                          
023900*    --- PARAMETRAR TILL WWOMVAND                                         
024000 01  FILLER                      PIC X(16) VALUE 'WWOMVAND'.              
024100*01  -COPY WWOMVAND                                                       
024200                                                                          
024300*    --- AREOR FÖR KOMMUNIKATION                                          
024400                                                                          
024500 01  FILLER                      PIC X(16) VALUE 'SEND-AREA'.             
024600 01  SEND-AREA.                                                           
024700*    03  -COPY WZ01SEND                                                   
024800                                                                          
024900 01  HDR-AREA.                                                            
025000*    03  -COPY WZ01REQU                                                   
025100*    03  -COPY WZ04HDR                                                    
025200                                                                          
025300 01  SEND-RAD.                                                            
025400     03  STYRTECKEN-RAD          PIC X.                                   
025500     03  FILLER                  PIC X(120) VALUE SPACE.                  
025600                                                                          
025700 01  DEL-NOTE-AREA-START         PIC X(24) VALUE                          
025800                                       'DEL-NOTE-AREA-START'.             
025900                                                                          
026000******* N D C -  D E L I V E R Y   N O T E ****************               
026100 01  DELIVERY-NOTE.                                                       
026200     03  DN-RUBRIK-1.                                                     
026300         05  FILLER              PIC X(03)   VALUE SPACE.                 
026400         05  DN-RUB1-NAMN        PIC X(36)   VALUE SPACE.                 
026500         05  FILLER              PIC X(36)   VALUE SPACE.                 
026600         05  DN-RUB1-SIDNR       PIC Z(02)9.                              
026700         05  FILLER              PIC X(12)   VALUE SPACE.                 
026800                                                                          
026900     03  DN-RUBRIK-2.                                                     
027000         05  FILLER              PIC X(03)   VALUE SPACE.                 
027100         05  DN-RUB2-IDDISTR     PIC Z(04)9.                              
027200         05  FILLER              PIC X(06)   VALUE SPACE.                 
027300         05  DN-RUB2-IDKUNDNR    PIC Z(06)9.                              
027400         05  FILLER              PIC X(05)   VALUE SPACE.                 
027500         05  DN-RUB2-IDORDNR     PIC Z(04)9.                              
027600         05  FILLER              PIC X(07)   VALUE SPACE.                 
027700         05  DN-RUB2-KDORDKL     PIC 9(01).                               
027800         05  FILLER              PIC X(10)   VALUE SPACE.                 
027900         05  DN-RUB2-IDPLOCK     PIC Z(06)9.                              
028000         05  FILLER              PIC X(04)   VALUE SPACE.                 
028100         05  DN-RUB2-IDPRODNR    PIC Z(06)9.                              
028200         05  FILLER              PIC X(13)   VALUE SPACE.                 
028300                                                                          
028400     03  DN-RUBRIK-3.                                                     
028500         05  FILLER              PIC X(03)   VALUE SPACE.                 
028600         05  DN-RUB3-ADRESS      PIC X(07)   VALUE SPACE.                 
028700         05  FILLER              PIC X(31)   VALUE SPACE.                 
028800         05  DN-RUB3-DC-HOME     PIC X(07)   VALUE SPACE.                 
028900         05  FILLER              PIC X(03)   VALUE SPACE.                 
029000         05  DN-RUB3-DC-DEL      PIC X(06)   VALUE SPACE.                 
029100         05  FILLER              PIC X(06)   VALUE SPACE.                 
029200         05  DN-RUB3-REGDAT-TID  PIC X(13)   VALUE SPACE.                 
029300         05  FILLER              PIC X(04)   VALUE SPACE.                 
029400                                                                          
029500     03  DN-RUBRIK-4.                                                     
029600         05  FILLER              PIC X(03)   VALUE SPACE.                 
029700         05  DN-RUB4-BEGMT-RAD1  PIC X(35).                               
029800         05  FILLER              PIC X(03)   VALUE SPACE.                 
029900         05  DN-RUB4-IDDC-HOME   PIC X(02)   VALUE SPACE.                 
030000         05  FILLER              PIC X(08)   VALUE SPACE.                 
030100         05  DN-RUB4-IDDC-LEV    PIC X(02).                               
030200         05  FILLER              PIC X(10)   VALUE SPACE.                 
030300         05  DN-RUB4-DAREGDAT    PIC 9(08).                               
030400         05  FILLER              PIC X(01)   VALUE SPACE.                 
030500         05  DN-RUB4-TIREGTID-HH PIC 9(02).                               
030600         05  FILLER              PIC X(01)   VALUE ':'.                   
030700         05  DN-RUB4-TIREGTID-MM PIC 9(02).                               
030800         05  FILLER              PIC X(05)   VALUE SPACE.                 
030900                                                                          
031000     03  DN-RUBRIK-5.                                                     
031100         05  FILLER              PIC X(03)   VALUE SPACE.                 
031200         05  DN-RUB5-BEGMT-RAD2  PIC X(35).                               
031300         05  FILLER              PIC X(41)   VALUE SPACE.                 
031400                                                                          
031500     03  DN-RUBRIK-6.                                                     
031600         05  FILLER              PIC X(03)   VALUE SPACE.                 
031700         05  DN-RUB6-ADGMT-GATA  PIC X(35).                               
031800         05  FILLER              PIC X(03)   VALUE SPACE.                 
031900         05  DN-RUB6-FC          PIC X(02)   VALUE SPACE.                 
032000         05  FILLER              PIC X(08)   VALUE SPACE.                 
032100         05  DN-RUB6-CUST-REF    PIC X(08)   VALUE SPACE.                 
032200         05  FILLER              PIC X(04)   VALUE SPACE.                 
032300         05  DN-RUB6-PRINTDATE   PIC X(10)   VALUE SPACE.                 
032400         05  FILLER              PIC X(08)   VALUE SPACE.                 
032500                                                                          
032600     03  DN-RUBRIK-7.                                                     
032700         05  FILLER              PIC X(03)   VALUE SPACE.                 
032800         05  DN-RUB7-ADGMT-PADR  PIC X(35).                               
032900         05  FILLER              PIC X(03)   VALUE SPACE.                 
033000         05  DN-RUB7-KDFRAKT     PIC Z(01)9.                              
033100         05  FILLER              PIC X(08)   VALUE SPACE.                 
033200         05  DN-RUB7-BEKUNDRF    PIC X(10).                               
033300         05  FILLER              PIC X(02)   VALUE SPACE.                 
033400         05  DN-RUB7-DAUTSKR     PIC X(08).                               
033500         05  FILLER              PIC X(01)   VALUE SPACE.                 
033600         05  DN-RUB7-TIPRT-HH    PIC X(02).                               
033700         05  FILLER              PIC X(01)   VALUE ':'.                   
033800         05  DN-RUB7-TIPRT-MM    PIC X(02).                               
033900         05  FILLER              PIC X(03)   VALUE SPACE.                 
034000                                                                          
034100     03  DN-RUBRIK-8.                                                     
034200         05  FILLER              PIC X(03)   VALUE SPACE.                 
034300         05  DN-RUB8-ADGMT-LAND  PIC X(35).                               
034400         05  FILLER              PIC X(42)   VALUE SPACE.                 
034500                                                                          
034600     03  DN-RUBRIK-9A.                                                    
034700         05  FILLER              PIC X(50)   VALUE SPACE.                 
034800         05  DN-RUB9A-CODE       PIC X(04)   VALUE SPACE.                 
034900         05  FILLER              PIC X(26)   VALUE SPACE.                 
035000                                                                          
035100     03  DN-RUBRIK-9B.                                                    
035200         05  FILLER              PIC X(28)   VALUE SPACE.                 
035300         05  DN-RUB9B-DELQTY     PIC X(03)   VALUE SPACE.                 
035400         05  FILLER              PIC X(05)   VALUE SPACE.                 
035500         05  DN-RUB9B-ORDQTY     PIC X(03)   VALUE SPACE.                 
035600         05  FILLER              PIC X(04)   VALUE SPACE.                 
035700         05  DN-RUB9B-BACKQTY    PIC X(04)   VALUE SPACE.                 
035800         05  FILLER              PIC X(03)   VALUE SPACE.                 
035900         05  DN-RUB9B-CODE       PIC X(04)   VALUE SPACE.                 
036000         05  FILLER              PIC X(05)   VALUE SPACE.                 
036100         05  DN-RUB9B-CASE       PIC X(04)   VALUE SPACE.                 
036200         05  FILLER              PIC X(12)   VALUE SPACE.                 
036300         05  DN-RUB9B-O-REF      PIC X(03)   VALUE SPACE.                 
036400         05  FILLER              PIC X(01)   VALUE SPACE.                 
036500                                                                          
036600     03  DN-RUBRIK-10.                                                    
036700         05  FILLER              PIC X(04)   VALUE SPACE.                 
036800         05  DN-RUB10-PARTNO     PIC X(08)   VALUE SPACE.                 
036900         05  FILLER              PIC X(01)   VALUE SPACE.                 
037000         05  DN-RUB10-PART-NAME  PIC X(05)   VALUE SPACE.                 
037100         05  FILLER              PIC X(10)   VALUE SPACE.                 
037200         05  DN-RUB10-DELQTY     PIC X(03)   VALUE SPACE.                 
037300         05  FILLER              PIC X(05)   VALUE SPACE.                 
037400         05  DN-RUB10-ORDQTY     PIC X(03)   VALUE SPACE.                 
037500         05  FILLER              PIC X(05)   VALUE SPACE.                 
037600         05  DN-RUB10-BACKQTY    PIC X(05)   VALUE SPACE.                 
037700         05  FILLER              PIC X(01)   VALUE SPACE.                 
037800         05  DN-RUB10-CODE       PIC X(04)   VALUE SPACE.                 
037900         05  FILLER              PIC X(01)   VALUE SPACE.                 
038000         05  DN-RUB10-DC         PIC X(02)   VALUE SPACE.                 
038100         05  FILLER              PIC X(02)   VALUE SPACE.                 
038200         05  DN-RUB10-CASE       PIC X(04)   VALUE SPACE.                 
038300         05  FILLER              PIC X(01)   VALUE SPACE.                 
038400         05  DN-RUB10-LOCATION   PIC X(08)   VALUE SPACE.                 
038500         05  FILLER              PIC X(03)   VALUE SPACE.                 
038600         05  DN-RUB10-O-REF      PIC X(03)   VALUE SPACE.                 
038700         05  FILLER              PIC X(01)   VALUE SPACE.                 
038800                                                                          
038900                                                                          
039000     03  DN-DETALJRAD.                                                    
039100         05  FILLER              PIC X(01)   VALUE SPACE.                 
039200         05  DN-RAD-IDARTNR      PIC Z(09).                               
039300         05  DN-RAD-STRECK       PIC X(01)   VALUE SPACE.                 
039400         05  DN-RAD-REKSIFFR     PIC X(01).                               
039500         05  FILLER              PIC X(01)   VALUE SPACE.                 
039600         05  DN-RAD-BEART-USA    PIC X(11)   VALUE SPACE.                 
039700         05  DN-RAD-KVLEVART     PIC Z(07).                               
039800         05  FILLER              PIC X(01)   VALUE SPACE.                 
039900         05  DN-RAD-KVBEART      PIC Z(07).                               
040000         05  FILLER              PIC X(01)   VALUE SPACE.                 
040100         05  DN-RAD-KVRO         PIC Z(07).                               
040200         05  FILLER              PIC X(03)   VALUE SPACE.                 
040300         05  DN-RAD-KDORDBEK     PIC X(03).                               
040400         05  FILLER              PIC X(02)   VALUE SPACE.                 
040500         05  DN-RAD-IDDC         PIC X(02).                               
040600         05  FILLER              PIC X(01)   VALUE SPACE.                 
040700         05  DN-RAD-IDKOLLI      PIC Z(05).                               
040800         05  FILLER              PIC X(01)   VALUE SPACE.                 
040900         05  DN-RAD-BERADREF     PIC X(10).                               
041000         05  FILLER              PIC X(01)   VALUE SPACE.                 
041100         05  DN-RAD-IDKUNDRF-RO  PIC Z(05).                               
041200         05  FILLER              PIC X(01)   VALUE SPACE.                 
041300                                                                          
041400     03  DN-SLUTRAD-1.                                                    
041500         05  FILLER              PIC X(03)    VALUE SPACE.                
041600         05  DN-SRAD1-GR-WEIGHT  PIC X(10)    VALUE SPACE.                
041700         05  FILLER              PIC X(06)    VALUE SPACE.                
041800         05  DN-SRAD1-VKORDBTO   PIC Z(05)Z.Z.                            
041900         05  FILLER              PIC X(01)    VALUE SPACE.                
042000         05  DN-SRAD1-VIKT       PIC X(06)    VALUE SPACE.                
042100         05  FILLER              PIC X(46)    VALUE SPACE.                
042200                                                                          
042300     03  DN-SLUTRAD-2.                                                    
042400         05  FILLER              PIC X(03)    VALUE SPACE.                
042500         05  DN-SRAD2-GR-CUBE    PIC X(10)    VALUE SPACE.                
042600         05  FILLER              PIC X(08)    VALUE SPACE.                
042700         05  DN-SRAD2-VLORDBTO   PIC Z(03)Z.Z(3).                         
042800         05  FILLER              PIC X(01)    VALUE SPACE.                
042900         05  DN-SRAD2-KUBIK      PIC X(03)    VALUE SPACE.                
043000         05  FILLER              PIC X(49)    VALUE SPACE.                
043100                                                                          
043200     03  DN-SLUTRAD-3.                                                    
043300         05  FILLER              PIC X(03)    VALUE SPACE.                
043400         05  DN-SRAD3-LINES      PIC X(18)    VALUE SPACE.                
043500         05  FILLER              PIC X(2)     VALUE SPACE.                
043600         05  DN-SRAD3-SUM-LINES  PIC Z(3)Z.                               
043700         05  FILLER              PIC X(53)    VALUE SPACE.                
043800                                                                          
043900     03  DN-SLUTRAD-4.                                                    
044000         05  FILLER              PIC X(03)    VALUE SPACE.                
044100         05  DN-SRAD4-CASES      PIC X(12)    VALUE SPACE.                
044200         05  FILLER              PIC X(6)     VALUE SPACE.                
044300         05  DN-SRAD4-SUM-CASES  PIC Z(5)Z.                               
044400         05  FILLER              PIC X(53)    VALUE SPACE.                
044500                                                                          
044600     03  DN-ERROR-LINE.                                                   
044700         05  FILLER              PIC X(03)    VALUE SPACE.                
044800         05  DN-ERROR-TEXT       PIC X(77)    VALUE SPACE.                
044900                                                                          
045000     03  BLANKRAD.                                                        
045100         05  FILLER              PIC X(121)   VALUE SPACE.                
045200                                                                          
045300     EJECT                                                                
045400                                                                          
045500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
045600*                                                                         
045700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
045800     SKIP3                                                                
045900*01  MID -COPY W4I34901                                                   
046000     EJECT                                                                
046100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
046200     SKIP3                                                                
046300*01  -COPY WMSGAREA                                                       
046400     EJECT                                                                
046500                                                                          
046600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
046700*                                                                         
046800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
046900     SKIP3                                                                
047000 01  NYCKLAR-TILL-DLI.                                                    
047100     03  W-IDPRODNR-X.                                                    
047200         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
047300                                                                          
047400     03  W-WDQ501KY-X.                                                    
047500         05  W-RADB-IDORDER      PIC S9(7) COMP-3 VALUE ZERO.             
047600         05  W-RADB-IDARTNR      PIC S9(9) COMP-3 VALUE ZERO.             
047700         05  W-RADB-IDLOPNR      PIC S9(3) COMP-3 VALUE ZERO.             
047800         05  W-RADB-IDSEKVNR     PIC S9(3) COMP-3 VALUE ZERO.             
047900         05  W-RADB-IDDC         PIC  X(2) VALUE SPACE.                   
048000         05  W-RADB-KDORDBEK     PIC  9(2) VALUE ZERO.                    
048100                                                                          
048200     03  W-WDQ501KY-MIN-X.                                                
048300         05  W-RADB-IDORDER-MIN  PIC S9(7) COMP-3 VALUE ZERO.             
048400         05  W-RADB-IDARTNR-MIN  PIC S9(9) COMP-3 VALUE ZERO.             
048500         05  W-RADB-IDLOPNR-MIN  PIC S9(3) COMP-3 VALUE ZERO.             
048600         05  W-RADB-IDSEKVNR-MIN PIC S9(3) COMP-3 VALUE ZERO.             
048700         05  W-RADB-IDDC-MIN     PIC  X(2) VALUE SPACE.                   
048800         05  W-RADB-KDORDBEK-MIN PIC  9(2) VALUE ZERO.                    
048900                                                                          
049000     03  W-WDQ501KY-MAX-X.                                                
049100         05  W-RADB-IDORDER-MAX  PIC S9(7) COMP-3 VALUE ZERO.             
049200         05  W-RADB-IDARTNR-MAX  PIC S9(9) COMP-3 VALUE +99999999.        
049300         05  W-RADB-IDLOPNR-MAX  PIC S9(3) COMP-3 VALUE +999.             
049400         05  W-RADB-IDSEKVNR-MAX PIC S9(3) COMP-3 VALUE +999.             
049500         05  W-RADB-IDDC-MAX     PIC  X(2) VALUE HIGH-VALUE.              
049600         05  W-RADB-KDORDBEK-MAX PIC  9(2) VALUE 99.                      
049700                                                                          
049800     03  W-WDQ501KY-HUV-MIN-X.                                            
049900         05  W-RADB-IDORDER-HUV-MIN  PIC S9(7) COMP-3                     
050000                                               VALUE ZERO.                
050100         05  W-RADB-IDARTNR-HUV-MIN  PIC S9(9) COMP-3                     
050200                                               VALUE ZERO.                
050300         05  W-RADB-IDLOPNR-HUV-MIN  PIC S9(3) COMP-3                     
050400                                               VALUE +1.                  
050500         05  W-RADB-IDSEKVNR-HUV-MIN PIC S9(3) COMP-3                     
050600                                               VALUE +1.                  
050700         05  W-RADB-IDDC-HUV-MIN     PIC  X(2) VALUE SPACE.               
050800         05  W-RADB-KDORDBEK-HUV-MIN PIC  9(2) VALUE ZERO.                
050900                                                                          
051000     03  W-WDQ501KY-HUV-MAX-X.                                            
051100         05  W-RADB-IDORDER-HUV-MAX  PIC S9(7) COMP-3                     
051200                                               VALUE ZERO.                
051300         05  W-RADB-IDARTNR-HUV-MAX  PIC S9(9) COMP-3                     
051400                                               VALUE ZERO.                
051500         05  W-RADB-IDLOPNR-HUV-MAX  PIC S9(3) COMP-3                     
051600                                               VALUE +1.                  
051700         05  W-RADB-IDSEKVNR-HUV-MAX PIC S9(3) COMP-3                     
051800                                               VALUE +1.                  
051900         05  W-RADB-IDDC-HUV-MAX     PIC  X(2) VALUE                      
052000                                               HIGH-VALUE.                
052100         05  W-RADB-KDORDBEK-HUV-MAX PIC  9(2) VALUE ZERO.                
052200                                                                          
052300     03  W-WDQ501KY-TILLK-MIN-X.                                          
052400         05  W-RADB-IDORDER-TILLK-MIN  PIC S9(7) COMP-3                   
052500                                                 VALUE ZERO.              
052600         05  W-RADB-IDARTNR-TILLK-MIN  PIC S9(9) COMP-3                   
052700                                                 VALUE ZERO.              
052800         05  W-RADB-IDLOPNR-TILLK-MIN  PIC S9(3) COMP-3                   
052900                                                 VALUE +001.              
053000         05  W-RADB-IDSEKVNR-TILLK-MIN PIC S9(3) COMP-3                   
053100                                                 VALUE +001.              
053200         05  W-RADB-IDDC-TILLK-MIN     PIC  X(2) VALUE SPACE.             
053300         05  W-RADB-KDORDBEK-TILLK-MIN PIC  9(2) VALUE ZERO.              
053400                                                                          
053500     03  W-WDQ501KY-TILLK-MAX-X.                                          
053600         05  W-RADB-IDORDER-TILLK-MAX  PIC S9(7) COMP-3                   
053700                                                 VALUE ZERO.              
053800         05  W-RADB-IDARTNR-TILLK-MAX  PIC S9(9) COMP-3                   
053900                                                 VALUE ZERO.              
054000         05  W-RADB-IDLOPNR-TILLK-MAX  PIC S9(3) COMP-3                   
054100                                                 VALUE +999.              
054200         05  W-RADB-IDSEKVNR-TILLK-MAX PIC S9(3) COMP-3                   
054300                                                 VALUE +999.              
054400         05  W-RADB-IDDC-TILLK-MAX     PIC  X(2) VALUE                    
054500                                                 HIGH-VALUE.              
054600         05  W-RADB-KDORDBEK-TILLK-MAX PIC  9(2) VALUE ZERO.              
054700                                                                          
054800     03  W-WDQ501KY-VIPSTILLK-MIN-X.                                      
054900         05  W-RADB-IDORDER-VIPSTILLK-MIN PIC S9(7)                       
055000                                          COMP-3 VALUE ZERO.              
055100         05  W-RADB-IDARTNR-VIPSTILLK-MIN PIC S9(9)                       
055200                                          COMP-3 VALUE ZERO.              
055300         05  W-RADB-IDLOPNR-VIPSTILLK-MIN PIC S9(3)                       
055400                                          COMP-3 VALUE +001.              
055500         05  W-RADB-IDSEKVNR-VIPSTILLK-MIN PIC S9(3)                      
055600                                          COMP-3 VALUE +001.              
055700         05  W-RADB-IDDC-VIPSTILLK-MIN PIC  X(2) VALUE SPACE.             
055800         05  W-RADB-KDORDBEK-VIPSTILLK-MIN PIC 9(2) VALUE ZERO.           
055900                                                                          
056000     03  W-WDQ501KY-VIPSTILLK-MAX-X.                                      
056100         05  W-RADB-IDORDER-VIPSTILLK-MAX PIC S9(7)                       
056200                                          COMP-3 VALUE ZERO.              
056300         05  W-RADB-IDARTNR-VIPSTILLK-MAX PIC S9(9)                       
056400                                          COMP-3 VALUE ZERO.              
056500         05  W-RADB-IDLOPNR-VIPSTILLK-MAX PIC S9(3)                       
056600                                          COMP-3 VALUE +999.              
056700         05  W-RADB-IDSEKVNR-VIPSTILLK-MAX PIC S9(3)                      
056800                                          COMP-3 VALUE +999.              
056900         05  W-RADB-IDDC-VIPSTILLK-MAX PIC  X(2) VALUE                    
057000                                                 HIGH-VALUE.              
057100         05  W-RADB-KDORDBEK-VIPSTILLK-MAX PIC 9(2) VALUE ZERO.           
057200                                                                          
057300     03  W-WDQ101KY-X.                                                    
057400         05  W-OBKR-IDORDER      PIC S9(7) COMP-3 VALUE ZERO.             
057500         05  W-OBKR-IDARTNR      PIC S9(9) COMP-3 VALUE ZERO.             
057600         05  W-OBKR-IDLOPNR      PIC S9(3) COMP-3 VALUE ZERO.             
057700         05  W-OBKR-IDSEKVNR     PIC S9(3) COMP-3 VALUE ZERO.             
057800         05  W-OBKR-IDDC         PIC  X(2) VALUE SPACE.                   
057900         05  W-OBKR-KDORDBEK     PIC  9(2) VALUE ZERO.                    
058000                                                                          
058100     03  W-WDQ101KY-MIN-X.                                                
058200         05  W-OBKR-IDORDER-MIN  PIC S9(7) COMP-3 VALUE ZERO.             
058300         05  W-OBKR-IDARTNR-MIN  PIC S9(9) COMP-3 VALUE ZERO.             
058400         05  W-OBKR-IDLOPNR-MIN  PIC S9(3) COMP-3 VALUE ZERO.             
058500         05  W-OBKR-IDSEKVNR-MIN PIC S9(3) COMP-3 VALUE ZERO.             
058600         05  W-OBKR-IDDC-MIN     PIC  X(2) VALUE SPACE.                   
058700         05  W-OBKR-KDORDBEK-MIN PIC  9(2) VALUE ZERO.                    
058800                                                                          
058900     03  W-WDQ101KY-MAX-X.                                                
059000         05  W-OBKR-IDORDER-MAX  PIC S9(7) COMP-3 VALUE ZERO.             
059100         05  W-OBKR-IDARTNR-MAX  PIC S9(9) COMP-3 VALUE ZERO.             
059200         05  W-OBKR-IDLOPNR-MAX  PIC S9(3) COMP-3 VALUE +999.             
059300         05  W-OBKR-IDSEKVNR-MAX PIC S9(3) COMP-3 VALUE +999.             
059400         05  W-OBKR-IDDC-MAX     PIC  X(2) VALUE HIGH-VALUE.              
059500         05  W-OBKR-KDORDBEK-MAX PIC  9(2) VALUE 99.                      
059600                                                                          
059700     03  W-WDQ101KY-LEV-MIN-X.                                            
059800         05  W-OBKR-IDORDER-LEV-MIN    PIC S9(7) COMP-3                   
059900                                                 VALUE ZERO.              
060000         05  W-OBKR-IDARTNR-LEV-MIN    PIC S9(9) COMP-3                   
060100                                                 VALUE ZERO.              
060200         05  W-OBKR-IDLOPNR-LEV-MIN    PIC S9(3) COMP-3                   
060300                                                 VALUE ZERO.              
060400         05  W-OBKR-IDSEKVNR-LEV-MIN PIC S9(3) COMP-3                     
060500                                                 VALUE ZERO.              
060600         05  W-OBKR-IDDC-LEV-MIN       PIC  X(2) VALUE SPACE.             
060700         05  W-OBKR-KDORDBEK-LEV-MIN PIC    9(2) VALUE ZERO.              
060800                                                                          
060900     03  W-WDQ101KY-LEV-MAX-X.                                            
061000         05  W-OBKR-IDORDER-LEV-MAX    PIC S9(7) COMP-3                   
061100                                                 VALUE ZERO.              
061200         05  W-OBKR-IDARTNR-LEV-MAX    PIC S9(9) COMP-3                   
061300                                                 VALUE ZERO.              
061400         05  W-OBKR-IDLOPNR-LEV-MAX    PIC S9(3) COMP-3                   
061500                                                 VALUE +999.              
061600         05  W-OBKR-IDSEKVNR-LEV-MAX PIC S9(3) COMP-3                     
061700                                                 VALUE +999.              
061800         05  W-OBKR-IDDC-LEV-MAX       PIC  X(2) VALUE                    
061900                                                 HIGH-VALUE.              
062000         05  W-OBKR-KDORDBEK-LEV-MAX PIC    9(2) VALUE 99.                
062100                                                                          
062200     03  W-BENA-IDARTNR-X.                                                
062300         05  W-BENA-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
062400     03  W-IDSKYLT-X.                                                     
062500        05 W-IDSKYLT             PIC X(3)   VALUE 'USA'.                  
062600                                                                          
062700     03  W-ARTS-IDARTNR-X.                                                
062800         05  W-ARTS-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
062900     03  W-ARTS-IDDC-X.                                                   
063000        05 W-ARTS-IDDC           PIC X(2)   VALUE SPACE.                  
063100                                                                          
063200     03  W-IDKOLLI-X.                                                     
063300         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
063400                                                                          
063500     03  W-KDKOLLI-X.                                                     
063600         05  W-KDKOLLI           PIC X(8)    VALUE SPACE.                 
063700     03 W-IDBET-X.                                                        
063800         05 W-IDFTG              PIC  9(2).                               
063900         05 W-IDLANDX2           PIC  X(2).                               
064000         05 W-IDBETNR            PIC  X(5).                               
064100                                                                          
064200     03  W-WDGX4459-X.                                                    
064300         05  W-4459-IDHTYP       PIC X(4)     VALUE '4459'.               
064400         05  W-4459-IDDC         PIC X(2).                                
064500         05  W-4459-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
064600                                                                          
064700     03  W-WDGX4460-X.                                                    
064800         05  W-4460-IDDISTR      PIC S9(5)    VALUE ZERO COMP-3.          
064900         05  W-4460-IDKUNDNR     PIC S9(7)    VALUE ZERO COMP-3.          
065000                                                                          
065100     03  W-WDGX4460-MIN-X.                                                
065200         05  W-4460-IDDISTR-MIN  PIC S9(5)    VALUE ZERO COMP-3.          
065300         05  W-4460-IDKUNDNR-MIN PIC S9(7)    VALUE ZERO COMP-3.          
065400                                                                          
065500     03  W-WDGX4460-MAX-X.                                                
065600         05  W-4460-IDDISTR-MAX  PIC S9(5)    VALUE ZERO COMP-3.          
065700         05  W-4460-IDKUNDNR-MAX PIC S9(7)    VALUE ZERO COMP-3.          
065800*--------------------WDB2                                                 
065900     03  W-IDGMT-X.                                                       
066000         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
066100         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
066200                                                                          
066300     03  W-IDDC-B6-X.                                                     
066400         05 W-IDDC-B6                  PIC X(2).                          
066500                                                                          
066600*    --- STATUS-KOD FRÅN IMS                                              
066700 01  STATUS-WS                   PIC XX.                                  
066800     88  SEGMENT-FINNS                       VALUE '  '.                  
066900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
067000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
067100     88  BASEN-SLUT                          VALUE 'GB'.                  
067200                                                                          
067300 01  TILLK-RADB-STATUS-WS        PIC XX.                                  
067400     88  TILLK-RADB-SEGMENT-FINNS            VALUE '  '.                  
067500     88  TILLK-RADB-SEGMENT-SAKNAS           VALUE 'GE'.                  
067600     88  TILLK-RADB-BASEN-SLUT               VALUE 'GB'.                  
067700                                                                          
067800 01  VIPSTILLK-RADB-STATUS-WS    PIC XX.                                  
067900     88  VIPSTILLK-RADB-SEGMENT-FINNS        VALUE '  '.                  
068000     88  VIPSTILLK-RADB-SEGMENT-SAKNAS       VALUE 'GE'.                  
068100     88  VIPSTILLK-RADB-BASEN-SLUT           VALUE 'GB'.                  
068200                                                                          
068300 01  RADB-STATUS-WS              PIC XX.                                  
068400     88  RADB-SEGMENT-FINNS                  VALUE '  '.                  
068500     88  RADB-SEGMENT-SAKNAS                 VALUE 'GE'.                  
068600     88  RADB-BASEN-SLUT                     VALUE 'GB'.                  
068700                                                                          
068800 01  OBKR-STATUS-WS              PIC XX.                                  
068900     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
069000     88  OBKR-SEGMENT-SAKNAS                 VALUE 'GE'.                  
069100     88  OBKR-BASEN-SLUT                     VALUE 'GB'.                  
069200                                                                          
069300 01  OBKR-LEV-STATUS-WS          PIC XX.                                  
069400     88  OBKR-LEV-SEGMENT-FINNS              VALUE '  '.                  
069500     88  OBKR-LEV-SEGMENT-SAKNAS             VALUE 'GE'.                  
069600     88  OBKR-LEV-BASEN-SLUT                 VALUE 'GB'.                  
069700                                                                          
069800 01  GODK-STATUSKODER.                                                    
069900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
070000                                                                          
070100 01  SSA1                        PIC X(68).                               
070200 01  SSA2                        PIC X(68).                               
070300     EJECT                                                                
070400                                                                          
070500*    --- IMS FUNKTIONSKODER                                               
070600*01  -COPY W0003                                                          
070700     EJECT                                                                
070800*    ---  DLI INPUT-OUTPUT AREA                                           
070900                                                                          
071000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE601'.                      
071100 01  DLI-IO-WDE601.                                                       
071200*    03  -COPY WDE601                                                     
071300     EJECT                                                                
071400                                                                          
071500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE611'.                      
071600 01  DLI-IO-WDE611.                                                       
071700*    03  -COPY WDE611                                                     
071800                                                                          
071900 01  FILLER         PIC X(16) VALUE 'ORDERHUVUD'.                         
072000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-HUV'.                  
072100 01  DLI-IO-ORQP01-HUV.                                                   
072200*    03  -COPY WDQ501   -PRE  HUV-                                        
072300                                                                          
072400 01  FILLER         PIC X(16) VALUE 'KUNDORDERRAD'.                       
072500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01'.                      
072600 01  DLI-IO-ORQP01.                                                       
072700*    03  -COPY WDQ501                                                     
072800                                                                          
072900 01  FILLER         PIC X(24) VALUE 'LEVRAD PULSTILLK'.                   
073000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-TILLK'.                
073100 01  DLI-IO-ORQP01-TILLK.                                                 
073200*    03  -COPY WDQ501   -PRE  TILLK-                                      
073300                                                                          
073400 01  FILLER         PIC X(24) VALUE 'LEVRAD VIPSTILLK'.                   
073500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQP01-VIPSTILLK'.            
073600 01  DLI-IO-ORQP01-VIPSTILLK.                                             
073700*    03  -COPY WDQ501   -PRE  VIPSTILLK-                                  
073800                                                                          
073900 01  FILLER         PIC X(16) VALUE 'ORDERBEKR'.                          
074000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQM01'.                      
074100 01  DLI-IO-ORQM01.                                                       
074200*    03  -COPY WDQ101                                                     
074300                                                                          
074400 01  FILLER         PIC X(16) VALUE 'ORDERBEKR-LEV'.                      
074500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORQM01-LEV'.                  
074600 01  DLI-IO-ORQM01-LEV.                                                   
074700*    03  -COPY WDQ101   -PRE LEV-                                         
074800                                                                          
074900 01  FILLER         PIC X(24) VALUE 'DLI-IO-4459'.                        
075000 01  DLI-IO-4459.                                                         
075100*    03  -COPY WDGX4459                                                   
075200     EJECT                                                                
075300                                                                          
075400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WL445911'.                    
075500 01  DLI-IO-4460.                                                         
075600*    03  -COPY WDGX4460                                                   
075700     EJECT                                                                
075800                                                                          
075900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
076000 01  DLI-IO-ARTS11.                                                       
076100*    03  -COPY WDK711                                                     
076200     EJECT                                                                
076300                                                                          
076400 01  FILLER         PIC X(24) VALUE 'DLI-IO-BENA11'.                      
076500 01  DLI-IO-BENA11.                                                       
076600*    03  -COPY WDD311                                                     
076700     EJECT                                                                
076800 01  FILLER                      PIC X(16)   VALUE 'IO-WDB201'.           
076900 01  DLI-IO-AREA-WDB201.                                                  
077000     03  WDB201.                                                          
077100*        05  -COPY WDB201                                                 
077200                                                                          
077300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
077400 01   DLI-IO-AREA-B601.                                                   
077500*     03  -COPY WDB601                                                    
077600     EJECT                                                                
077700                                                                          
077800                                                                          
077900                                                                          
078000 LINKAGE SECTION.                                                         
078100*01  -COPY W0009   -PRE MSG-                                              
078200     EJECT                                                                
078300                                                                          
078400*01  -COPY W0009   -PRE DISTRDOC-                                         
078500     EJECT                                                                
078600                                                                          
078700*01  -COPY W0009   -PRE ALT-                                              
078800     EJECT                                                                
078900                                                                          
079000*01  -COPY W0008  -PRE WDE6-                                              
079100     05  FILLER                  PIC X.                                   
079200     EJECT                                                                
079300                                                                          
079400* ORQP-PCB ANVÄNDS BÅDE FÖR "HUVUD" OCH VIPSTILLKOMMANDE                  
079500*01  -COPY W0008  -PRE ORQP-                                              
079600     05  FILLER                  PIC X.                                   
079700     EJECT                                                                
079800                                                                          
079900*01  -COPY W0008  -PRE ORQP2-                                             
080000     05  FILLER                  PIC X.                                   
080100     EJECT                                                                
080200                                                                          
080300*01  -COPY W0008  -PRE ORQP3-                                             
080400     05  FILLER                  PIC X.                                   
080500     EJECT                                                                
080600                                                                          
080700*01  -COPY W0008  -PRE ORQM-                                              
080800     05  FILLER                  PIC X.                                   
080900     EJECT                                                                
081000                                                                          
081100*01  -COPY W0008  -PRE ORQM2-                                             
081200     05  FILLER                  PIC X.                                   
081300     EJECT                                                                
081400                                                                          
081500*01  -COPY W0008  -PRE 4459-                                              
081600     05  FILLER                  PIC X.                                   
081700     EJECT                                                                
081800                                                                          
081900*01  -COPY W0008  -PRE ARTS-                                              
082000     05  FILLER                  PIC X.                                   
082100     EJECT                                                                
082200                                                                          
082300*01  -COPY W0008  -PRE BENA-                                              
082400     05  FILLER                  PIC X.                                   
082500     EJECT                                                                
082600                                                                          
082700*01  -COPY W0008  -PRE WDB2-                                              
082800     05  FILLER                  PIC X.                                   
082900     EJECT                                                                
083000                                                                          
083100*01  -COPY W0008  -PRE WDB6-                                              
083200     05  FILLER                  PIC X.                                   
083300     EJECT                                                                
083400                                                                          
083500 PROCEDURE DIVISION  USING MSG-PCB                                        
083600                           DISTRDOC-PCB                                   
083700                           ALT-PCB                                        
083800                           WDE6-PCB                                       
083900                           ORQP-PCB                                       
084000                           ORQP2-PCB                                      
084100                           ORQP3-PCB                                      
084200                           ORQM-PCB                                       
084300                           ORQM2-PCB                                      
084400                           4459-PCB                                       
084500                           ARTS-PCB                                       
084600                           BENA-PCB                                       
084700                           WDB2-PCB                                       
084800                           WDB6-PCB.                                      
084900 MAIN SECTION.                                                            
085000     ENTRY 'DLITCBL' USING MSG-PCB                                        
085100                           DISTRDOC-PCB                                   
085200                           ALT-PCB                                        
085300                           WDE6-PCB                                       
085400                           ORQP-PCB                                       
085500                           ORQP2-PCB                                      
085600                           ORQP3-PCB                                      
085700                           ORQM-PCB                                       
085800                           ORQM2-PCB                                      
085900                           4459-PCB                                       
086000                           ARTS-PCB                                       
086100                           BENA-PCB                                       
086200                           WDB2-PCB                                       
086300                           WDB6-PCB.                                      
086400                                                                          
086500                                                                          
086600     MOVE 'STA-HUV '      TO FELTEXT                                      
086700                                                                          
086800     PERFORM IMS-GET-MSG                                                  
086900     IF SEGMENT-FINNS                                                     
087000        PERFORM A-INIT                                                    
087100        PERFORM C-KOLLA-DATA                                              
087200        IF INDATA-OK                                                      
087300           PERFORM D-STARTA-LISTA                                         
087400           PERFORM E-DN-HUVUD                                             
087500           PERFORM F-DN-DETALJRADER                                       
087600           PERFORM G-DN-SLUTRADER                                         
087700           PERFORM H-AVSLUTA-LISTA                                        
087800        END-IF                                                            
087900     END-IF                                                               
088000     MOVE ZERO TO RETURN-CODE                                             
088100     GOBACK                                                               
088200     .                                                                    
088300     EJECT                                                                
088400                                                                          
088500 A-INIT SECTION.                                                          
088600                                                                          
088700     MOVE 'STA-A   '      TO FELTEXT                                      
088800                                                                          
088900     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
089000     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I34901                    
089100     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
089200                                                                          
089300     INSPECT MID-IDDISTR  REPLACING LEADING SPACE BY ZERO                 
089400     INSPECT MID-IDORDNR7 REPLACING LEADING SPACE BY ZERO                 
089500     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
089600     INSPECT MID-IDPRODNR REPLACING LEADING SPACE BY ZERO                 
089700     INSPECT MID-IDORDER  REPLACING LEADING SPACE BY ZERO                 
089800                                                                          
089900     MOVE MID-IDDISTR           TO W-4460-IDDISTR                         
090000                                   WS-IDDISTR                             
090100                                                                          
090200     MOVE MID-IDKUNDNR          TO W-4460-IDKUNDNR                        
090300                                   WS-IDKUNDNR                            
090400                                                                          
090500     MOVE MID-IDORDNR7          TO WS-IDORDNR                             
090600                                   WS-IDORDNR-X                           
090700                                                                          
090800     MOVE MID-IDPRODNR          TO WS-IDPRODNR                            
090900                                                                          
091000     MOVE MID-IDDC              TO W-4459-IDDC                            
091100                                   W-IDDC-B6                              
091200                                   PERFORM IMS-GU-WDB601                  
091300                                                                          
091400     MOVE MID-IDORDER           TO WS-IDORDER                             
091500     .                                                                    
091600     EJECT                                                                
091700                                                                          
091800 C-KOLLA-DATA SECTION.                                                    
091900                                                                          
092000     MOVE 'STA-C   '      TO FELTEXT                                      
092100                                                                          
092200     MOVE LOW-VALUE    TO W-WDQ501KY-HUV-MIN-X                            
092300     MOVE HIGH-VALUE   TO W-WDQ501KY-HUV-MAX-X                            
092400     MOVE WS-IDORDER   TO W-RADB-IDORDER-HUV-MIN                          
092500                          W-RADB-IDORDER-HUV-MAX                          
092600     MOVE ZERO         TO W-RADB-IDARTNR-HUV-MIN                          
092700                          W-RADB-IDARTNR-HUV-MAX                          
092800     PERFORM IMS-GU-RADB-HUV-MIN-MAX                                      
092900     IF SEGMENT-FINNS                                                     
093000        MOVE DCS-IDDC               TO WS-IDDC-LEV                        
093100                                                                          
093200        MOVE HUV-RADB-IDDISTR       TO W-IDDISTR                          
093300        MOVE HUV-RADB-IDKUNDNR      TO W-IDKUNDNR                         
093400        PERFORM IMS-GU-WDB201                                             
093500        IF HUV-RADB-KDORDKL > 1                                           
093600           MOVE GMT-IDDC-BULK (1)   TO WS-IDDC-HOME                       
093700        ELSE                                                              
093800          IF HUV-RADB-KDORDKL = 1                                         
093900             MOVE GMT-IDDC-DAY (1)  TO WS-IDDC-HOME                       
094000          ELSE                                                            
094100            IF HUV-RADB-KDORDKL = 0                                       
094200               MOVE GMT-IDDC-VOR(1) TO WS-IDDC-HOME                       
094300            END-IF                                                        
094400          END-IF                                                          
094500        END-IF                                                            
094600                                                                          
094700     ELSE                                                                 
094800        MOVE NEJ TO INDATA-SW                                             
094900     END-IF                                                               
095000                                                                          
095100     IF INDATA-OK                                                         
095200        MOVE WS-IDPRODNR            TO W-IDPRODNR                         
095300        PERFORM IMS-GU-WDE6-VORD                                          
095400        IF SEGMENT-FINNS                                                  
095500           MOVE JA                  TO ORDERDEL-UTSKRIVEN-SW              
095600           PERFORM IMS-GNP-WDE6-KOLLI                                     
095700           IF SEGMENT-FINNS                                               
095800              MOVE JA               TO PACKNING-STARTAD-SW                
095900           END-IF                                                         
096000        END-IF                                                            
096100     END-IF                                                               
096200     .                                                                    
096300     EJECT                                                                
096400                                                                          
096500 D-STARTA-LISTA SECTION.                                                  
096600                                                                          
096700     MOVE 'STA-D   '      TO FELTEXT                                      
096800                                                                          
096900                                                                          
097000     MOVE 'W40349-001'             TO PRT-IDLIST                          
097100     MOVE SPACE                    TO WS-DUMMY                            
097200*    FYLL WS-VALD-PRINTER                                                 
097300     PERFORM IMS-GU-4459-BARN                                             
097400     IF SEGMENT-FINNS                                                     
097500        MOVE 4460-IDPRTLST         TO WS-VALD-PRINTER                     
097600     ELSE                                                                 
097700        EVALUATE TRUE                                                     
097800           WHEN NDC-US-RU                                                 
097900              MOVE '4DN41001'      TO WS-VALD-PRINTER                     
098000           WHEN DCS-NDC-NA AND DCS-CANADA                                 
098100              MOVE '4DN51001'      TO WS-VALD-PRINTER                     
098200        END-EVALUATE                                                      
098300     END-IF                                                               
098400                                                                          
098500     MOVE MID-IDDC        TO WS-IDDC                                      
098600     IF GMT-FLDNDAP = NEJ AND NDC-NA                                      
098700        CONTINUE                                                          
098800     ELSE                                                                 
098900        IF GMT-FLDNDAP = JA                                               
099000        AND (HUV-RADB-KDORDKL = 0 OR 1 OR 2 OR 3 OR 4)                    
099100                                                                          
099200          PERFORM S90-SEND-OPEN                                           
099300        ELSE                                                              
099400          CALL W006PRS1 USING PRT-SPOOL-A4S                               
099500                              PRT-OPEN                                    
099600                              WS-VALD-PRINTER                             
099700                              ALT-PCB                                     
099800                              WS-DUMMY                                    
099900        END-IF                                                            
100000     END-IF                                                               
100100     .                                                                    
100200     EJECT                                                                
100300                                                                          
100400 E-DN-HUVUD SECTION.                                                      
100500                                                                          
100600     MOVE 'STA-E   '      TO FELTEXT                                      
100700                                                                          
100800     PERFORM EA-FLYTTA-RUBRIKTEXTER                                       
100900     PERFORM EB-FLYTTA-RUBRIKVARDEN                                       
101000     .                                                                    
101100     EJECT                                                                
101200                                                                          
101300 EA-FLYTTA-RUBRIKTEXTER SECTION.                                          
101400                                                                          
101500     MOVE 'STA-EA  '      TO FELTEXT                                      
101600                                                                          
101700     MOVE 'D E L I V E R Y  N O T E' TO DN-RUB1-NAMN                      
101800     MOVE 'ADDRESS'                  TO DN-RUB3-ADRESS                    
101900     MOVE 'HOME-DC'                  TO DN-RUB3-DC-HOME                   
102000     MOVE 'DEL-DC'                   TO DN-RUB3-DC-DEL                    
102100     MOVE 'REG DATE'                 TO DN-RUB3-REGDAT-TID                
102200     MOVE 'FC'                       TO DN-RUB6-FC                        
102300     MOVE 'CUST-REF'                 TO DN-RUB6-CUST-REF                  
102400     MOVE 'PRINT DATE'               TO DN-RUB6-PRINTDATE                 
102500     MOVE 'ORD.'                     TO DN-RUB9A-CODE                     
102600     MOVE 'DEL'                      TO DN-RUB9B-DELQTY                   
102700     MOVE 'ORD'                      TO DN-RUB9B-ORDQTY                   
102800     MOVE 'BACK'                     TO DN-RUB9B-BACKQTY                  
102900     MOVE 'CONF'                     TO DN-RUB9B-CODE                     
103000     MOVE 'CASE'                     TO DN-RUB9B-CASE                     
103100     MOVE 'ORD'                      TO DN-RUB9B-O-REF                    
103200     MOVE 'PART NO.'                 TO DN-RUB10-PARTNO                   
103300     MOVE 'DESCR'                    TO DN-RUB10-PART-NAME                
103400     MOVE 'QTY'                      TO DN-RUB10-DELQTY                   
103500     MOVE 'QTY'                      TO DN-RUB10-ORDQTY                   
103600     MOVE 'QTY'                      TO DN-RUB10-BACKQTY                  
103700     MOVE 'CODE'                     TO DN-RUB10-CODE                     
103800     MOVE 'DC'                       TO DN-RUB10-DC                       
103900     MOVE '  NO'                     TO DN-RUB10-CASE                     
104000     MOVE 'LOCATION'                 TO DN-RUB10-LOCATION                 
104100     MOVE 'REF'                      TO DN-RUB10-O-REF                    
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 EB-FLYTTA-RUBRIKVARDEN SECTION.                                          
104600                                                                          
104700     MOVE 'STA-EB  '      TO FELTEXT                                      
104800                                                                          
104900     PERFORM EBA-OMVANDLA-DATUM-TID                                       
105000                                                                          
105100     MOVE WS-IDDISTR               TO DN-RUB2-IDDISTR                     
105200     MOVE WS-IDKUNDNR              TO DN-RUB2-IDKUNDNR                    
105300     MOVE WS-IDORDNR-X(3:5)        TO DN-RUB2-IDORDNR                     
105400     MOVE HUV-RADB-KDORDKL         TO DN-RUB2-KDORDKL                     
105500     IF PACKNING-STARTAD                                                  
105600        MOVE KOLLI-IDPLOCK         TO DN-RUB2-IDPLOCK                     
105700     ELSE                                                                 
105800        MOVE ZERO                  TO DN-RUB2-IDPLOCK                     
105900     END-IF                                                               
106000     IF ORDERDEL-UTSKRIVEN                                                
106100        MOVE VORD-IDPRODNR         TO DN-RUB2-IDPRODNR                    
106200     ELSE                                                                 
106300        MOVE ZERO                  TO DN-RUB2-IDPRODNR                    
106400     END-IF                                                               
106500                                                                          
106600     MOVE HUV-RADB-BEGMT-RAD1      TO DN-RUB4-BEGMT-RAD1                  
106700     MOVE WS-IDDC-HOME             TO DN-RUB4-IDDC-HOME                   
106800     MOVE WS-IDDC-LEV              TO DN-RUB4-IDDC-LEV                    
106900     MOVE WS-DAREGDAT              TO DN-RUB4-DAREGDAT                    
107000     MOVE WS-TIREGTID-HH           TO DN-RUB4-TIREGTID-HH                 
107100     MOVE WS-TIREGTID-MM           TO DN-RUB4-TIREGTID-MM                 
107200                                                                          
107300     MOVE HUV-RADB-BEGMT-RAD2      TO DN-RUB5-BEGMT-RAD2                  
107400                                                                          
107500     MOVE HUV-RADB-ADGMT-GATA      TO DN-RUB6-ADGMT-GATA                  
107600                                                                          
107700     MOVE HUV-RADB-ADGMT-PADR      TO DN-RUB7-ADGMT-PADR                  
107800     MOVE HUV-RADB-KDFRAKT         TO DN-RUB7-KDFRAKT                     
107900     MOVE HUV-RADB-BEKUNDRF        TO DN-RUB7-BEKUNDRF                    
108000     IF ORDERDEL-UTSKRIVEN                                                
108100        MOVE WS-DAUTSKR               TO DN-RUB7-DAUTSKR                  
108200        MOVE WS-TIUTSTID-HH           TO DN-RUB7-TIPRT-HH                 
108300        MOVE WS-TIUTSTID-MM           TO DN-RUB7-TIPRT-MM                 
108400     ELSE                                                                 
108500        MOVE SPACE                 TO DN-RUB7-DAUTSKR                     
108600                                      DN-RUB7-TIPRT-HH                    
108700                                      DN-RUB7-TIPRT-MM                    
108800     END-IF                                                               
108900     MOVE HUV-RADB-ADGMT-LAND      TO DN-RUB8-ADGMT-LAND                  
109000     .                                                                    
109100     EJECT                                                                
109200                                                                          
109300 EBA-OMVANDLA-DATUM-TID SECTION.                                          
109400                                                                          
109500     MOVE 'STA-EBA '      TO FELTEXT                                      
109600                                                                          
109700     MOVE HUV-RADB-TIREGDAT        TO WS-TIREGDAT                         
109800                                      WS-DAREGDAT-YYMMDD                  
109900     IF WS-TIREGDAT-YY > 90                                               
110000        MOVE 19                    TO WS-DAREGDAT-SS                      
110100     ELSE                                                                 
110200        MOVE 20                    TO WS-DAREGDAT-SS                      
110300     END-IF                                                               
110400                                                                          
110500     MOVE HUV-RADB-TIREGTID        TO WS-TIREGTID                         
110600                                                                          
110700     MOVE VORD-TIUTSKR             TO WS-TIUTSKR                          
110800                                      WS-DAUTSKR-YYMMDD                   
110900     IF WS-TIUTSKR-YY > 90                                                
111000                                                                          
111100        MOVE 19                    TO WS-DAUTSKR-SS                       
111200     ELSE                                                                 
111300        MOVE 20                    TO WS-DAUTSKR-SS                       
111400     END-IF                                                               
111500                                                                          
111600     MOVE VORD-TIUTSTID            TO WS-TIUTSTID                         
111700     .                                                                    
111800     EJECT                                                                
111900                                                                          
112000 F-DN-DETALJRADER SECTION.                                                
112100                                                                          
112200     MOVE 'STA-F   '      TO FELTEXT                                      
112300                                                                          
112400     PERFORM S41-SKRIV-RUBRIKER                                           
112500     MOVE LOW-VALUE    TO W-WDQ501KY-MIN-X                                
112600     MOVE HIGH-VALUE   TO W-WDQ501KY-MAX-X                                
112700     MOVE WS-IDORDER   TO W-RADB-IDORDER-MIN                              
112800                          W-RADB-IDORDER-MAX                              
112900     MOVE +1           TO W-RADB-IDARTNR-MIN                              
113000                                                                          
113100     PERFORM IMS-GU-RADB-MIN-MAX                                          
113200     PERFORM UNTIL ((RADB-SEGMENT-SAKNAS)                                 
113300               OR   (RADB-BASEN-SLUT))                                    
113400      IF WS-RADRAK > WS-MAX-RADER                                         
113500         PERFORM S41-SKRIV-RUBRIKER                                       
113600      END-IF                                                              
113700      MOVE RADB-IDARTNR        TO WS-SPAR-IDARTNR                         
113800      PERFORM UNTIL ((RADB-SEGMENT-SAKNAS)                                
113900                OR   (RADB-BASEN-SLUT)                                    
114000                OR   (RADB-IDARTNR NOT = WS-SPAR-IDARTNR))                
114100       IF WS-RADRAK > WS-MAX-RADER                                        
114200          PERFORM S41-SKRIV-RUBRIKER                                      
114300       END-IF                                                             
114400       MOVE RADB-IDLOPNR        TO WS-SPAR-IDLOPNR                        
114500       MOVE RADB-IDPURAD        TO WS-SPAR-IDPURAD                        
114600       MOVE RADB-IDKOLLI        TO WS-SPAR-IDKOLLI                        
114700       PERFORM UNTIL ((RADB-SEGMENT-SAKNAS)                               
114800                 OR   (RADB-BASEN-SLUT)                                   
114900                 OR   (RADB-IDARTNR NOT = WS-SPAR-IDARTNR)                
115000                 OR   (RADB-IDPURAD NOT = WS-SPAR-IDPURAD))               
115100                                                                          
115200          PERFORM FX-NOLLSTALL                                            
115300          IF RADB-IDKOLLI = WS-SPAR-IDKOLLI                               
115400                                                                          
115500             IF (((RADB-IDDC  = WS-IDDC-HOME)       AND                   
115600                                                                          
115700                  ((RADB-FLTILLK = JA  AND                                
115800                   RADB-IDKUNDRF-RO(3:5)  = '00000')                      
115900                                                  OR                      
116000                   (RADB-FLTILLK = VIPS)))                                
116100                                                                          
116200             OR  ((RADB-IDDC NOT = WS-IDDC-LEV)     AND                   
116300                                                                          
116400                  ((RADB-FLTILLK = JA  AND                                
116500                    RADB-IDKUNDRF-RO(3:5)  = '00000')                     
116600                                                  OR                      
116700                   (RADB-FLTILLK = VIPS))))                               
116800                                                                          
116900                PERFORM FA-TILLKOMMANDE-HEMMALAGER                        
117000             ELSE                                                         
117100               IF RADB-KDORDBEK = 58 OR 61                                
117200                  PERFORM FB-VIPSKOMMENTAR                                
117300               ELSE                                                       
117400                  IF RADB-KDORDBEK = 41 AND                               
117500                     RADB-IDDC = WS-IDDC-LEV                              
117600                     PERFORM FC-VIPSERSATTNING                            
117700                  ELSE                                                    
117800                     PERFORM FD-KOLLA-PULS-OBKR                           
117900                     IF PULSERS                                           
118000                        PERFORM FF-PULSERSATTNING                         
118100                     ELSE                                                 
118200                        IF PULSOBKR                                       
118300                           PERFORM FG-PULSOBKR                            
118400                        ELSE                                              
118500                           PERFORM FH-LEVRAD                              
118600                        END-IF                                            
118700                     END-IF                                               
118800                  END-IF                                                  
118900               END-IF                                                     
119000            END-IF                                                        
119100          ELSE                                                            
119200            PERFORM FI-EXTRA-KOLLI-RAD                                    
119300          END-IF                                                          
119400          PERFORM IMS-GN-RADB-MIN-MAX                                     
119500       END-PERFORM                                                        
119600      END-PERFORM                                                         
119700     END-PERFORM                                                          
119800     .                                                                    
119900     EJECT                                                                
120000                                                                          
120100 FX-NOLLSTALL SECTION.                                                    
120200                                                                          
120300     MOVE 'STA-FX  '      TO FELTEXT                                      
120400                                                                          
120500     MOVE NEJ             TO TILLKOMMANDE-SW                              
120600     INITIALIZE WS-DN-DETALJRAD-LAST                                      
120700     .                                                                    
120800     EJECT                                                                
120900                                                                          
121000 FA-TILLKOMMANDE-HEMMALAGER SECTION.                                      
121100                                                                          
121200     MOVE 'STA-FA  '      TO FELTEXT                                      
121300                                                                          
121400     CONTINUE                                                             
121500     .                                                                    
121600     EJECT                                                                
121700                                                                          
121800 FB-VIPSKOMMENTAR   SECTION.                                              
121900                                                                          
122000     MOVE 'STA-FB  '      TO FELTEXT                                      
122100                                                                          
122200     IF RADB-IDDC = WS-IDDC-LEV                                           
122300        EVALUATE RADB-KDORDBEK                                            
122400                                                                          
122500           WHEN 58                                                        
122600            IF RADB-BEART-USA = SPACE                                     
122700               INITIALIZE DN-DETALJRAD                                    
122800               MOVE RADB-IDARTNR       TO DN-RAD-IDARTNR                  
122900               MOVE '-'                TO DN-RAD-STRECK                   
123000               MOVE RADB-REKSIFFR      TO DN-RAD-REKSIFFR                 
123100               MOVE 'UNKNOWN  '        TO DN-RAD-BEART-USA                
123200               MOVE RADB-KVBEART       TO DN-RAD-KVBEART                  
123300               MOVE RADB-KDORDBEK      TO DN-RAD-KDORDBEK                 
123400*              MOVE 'FB1 '             TO DN-RAD-BEART-USA(8:4)           
123500               PERFORM S42-SKRIV-DETALJRAD                                
123600            END-IF                                                        
123700                                                                          
123800           WHEN 61                                                        
123900            MOVE RADB-IDSEKVNR TO WS-IDSEKVNR                             
124000            IF WS-IDSEKVNR (3:1) = 1 OR 3 OR 5 OR 7 OR 9                  
124100               INITIALIZE DN-DETALJRAD                                    
124200               MOVE RADB-IDARTNR          TO DN-RAD-IDARTNR               
124300               MOVE '-'                   TO DN-RAD-STRECK                
124400               MOVE RADB-REKSIFFR         TO DN-RAD-REKSIFFR              
124500               MOVE RADB-BEART-USA        TO DN-RAD-BEART-USA             
124600               MOVE RADB-KVBEART          TO DN-RAD-KVBEART               
124700               MOVE RADB-KDORDBEK         TO DN-RAD-KDORDBEK              
124800               PERFORM S42-SKRIV-DETALJRAD                                
124900            ELSE                                                          
125000               INITIALIZE DN-DETALJRAD                                    
125100               MOVE RADB-BEART-USA       TO DN-RAD-BEART-USA              
125200               PERFORM S42-SKRIV-DETALJRAD                                
125300               PERFORM S29-SPARRTEXT                                      
125400            END-IF                                                        
125500                                                                          
125600        END-EVALUATE                                                      
125700     END-IF                                                               
125800     .                                                                    
125900     EJECT                                                                
126000                                                                          
126100 FC-VIPSERSATTNING SECTION.                                               
126200                                                                          
126300     MOVE 'STA-FC  '      TO FELTEXT                                      
126400                                                                          
126500     INITIALIZE DN-DETALJRAD                                              
126600     MOVE SPACE TO WS-KDORDBEK                                            
126700     IF RADB-IDARTNR-TILLK = ZERO                                         
126800                                                                          
126900        MOVE RADB-IDARTNR          TO DN-RAD-IDARTNR                      
127000        MOVE '-'                   TO DN-RAD-STRECK                       
127100        MOVE RADB-REKSIFFR         TO DN-RAD-REKSIFFR                     
127200        MOVE RADB-KVBEART          TO DN-RAD-KVBEART                      
127300        MOVE RADB-KDORDBEK         TO RED-KDORDBEK-KOD                    
127400        MOVE RADB-BEART-USA        TO DN-RAD-BEART-USA                    
127500        MOVE '*'                   TO RED-KDORDBEK-X                      
127600        MOVE RED-KDORDBEK          TO DN-RAD-KDORDBEK                     
127700        PERFORM S42-SKRIV-DETALJRAD                                       
127800        INITIALIZE DN-DETALJRAD                                           
127900        MOVE 'REPLACED BY'      TO DN-RAD-BEART-USA                       
128000        MOVE '*'                TO DN-RAD-KDORDBEK                        
128100        PERFORM S42-SKRIV-DETALJRAD                                       
128200     ELSE                                                                 
128300        PERFORM FC1-KOLLA-PULSOBKR-VIPSTILLK                              
128400        IF PULSERS-VIPSTILLK                                              
128500           PERFORM FC2-VIPSTILLK-PULSERS                                  
128600        ELSE                                                              
128700           IF PULSOBKR-VIPSTILLK                                          
128800              PERFORM FC3-VIPSTILLK-PULSOBKR                              
128900            ELSE                                                          
129000              PERFORM FC4-VIPSTILLK-LEVRAD                                
129100           END-IF                                                         
129200        END-IF                                                            
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600                                                                          
129700 FC1-KOLLA-PULSOBKR-VIPSTILLK SECTION.                                    
129800                                                                          
129900     MOVE 'STA-FC1 '         TO FELTEXT                                   
130000                                                                          
130100     MOVE LOW-VALUE           TO W-WDQ501KY-VIPSTILLK-MIN-X               
130200     MOVE HIGH-VALUE          TO W-WDQ501KY-VIPSTILLK-MAX-X               
130300     MOVE RADB-IDORDER        TO W-RADB-IDORDER-VIPSTILLK-MIN             
130400                                 W-RADB-IDORDER-VIPSTILLK-MAX             
130500     MOVE RADB-IDARTNR-TILLK  TO W-RADB-IDARTNR-VIPSTILLK-MIN             
130600                                 W-RADB-IDARTNR-VIPSTILLK-MAX             
130700                                                                          
130800     MOVE NEJ                 TO PULSERS-VIPSTILLK-SW                     
130900                                 PULSOBKR-VIPSTILLK-SW                    
131000                                                                          
131100     PERFORM IMS-GU-RADB-VIPSTILLK-MIN-MAX                                
131200     PERFORM UNTIL ((VIPSTILLK-RADB-SEGMENT-SAKNAS)                       
131300               OR   ( VIPSTILLK-RADB-BASEN-SLUT))                         
131400        IF VIPSTILLK-RADB-FLTILLK = VIPS                                  
131500           IF RADB-KVBEART = VIPSTILLK-RADB-KVBEART                       
131600                                                                          
131700              MOVE LOW-VALUE          TO W-WDQ101KY-MIN-X                 
131800              MOVE HIGH-VALUE         TO W-WDQ101KY-MAX-X                 
131900              MOVE RADB-IDORDER       TO W-OBKR-IDORDER-MIN               
132000                                         W-OBKR-IDORDER-MAX               
132100              MOVE VIPSTILLK-RADB-IDARTNR TO                              
132200                                      W-OBKR-IDARTNR-MIN                  
132300                                      W-OBKR-IDARTNR-MAX                  
132400              PERFORM IMS-GU-OBKR-MIN-MAX                                 
132500              PERFORM UNTIL ((OBKR-SEGMENT-SAKNAS)                        
132600                        OR   (OBKR-BASEN-SLUT))                           
132700                 IF VIPSTILLK-RADB-KVBEART = OBKR-KVBEART                 
132800                 OR VIPSTILLK-RADB-KVBEART-Q = OBKR-KVBEART-Q             
132900                  IF VIPSTILLK-RADB-IDKUNDRF-RO(3:5) =                    
133000                     OBKR-IDKUNDRF-RO(3:5)                                
133100                    IF OBKR-KDORDBEK = 41                                 
133200                       IF VIPSTILLK-RADB-IDDC = WS-IDDC-LEV               
133300                          MOVE JA TO PULSERS-VIPSTILLK-SW                 
133400                       END-IF                                             
133500                    ELSE                                                  
133600                       IF OBKR-KDORDBEK > ZERO                            
133700                          MOVE JA TO PULSOBKR-VIPSTILLK-SW                
133800                       END-IF                                             
133900                    END-IF                                                
134000                  END-IF                                                  
134100                 END-IF                                                   
134200                 PERFORM IMS-GN-OBKR-MIN-MAX                              
134300              END-PERFORM                                                 
134400           END-IF                                                         
134500        END-IF                                                            
134600        PERFORM IMS-GN-RADB-VIPSTILLK-MIN-MAX                             
134700     END-PERFORM                                                          
134800     .                                                                    
134900     EJECT                                                                
135000                                                                          
135100 FC2-VIPSTILLK-PULSERS SECTION.                                           
135200                                                                          
135300     MOVE 'STA-FC2 '      TO FELTEXT                                      
135400                                                                          
135500     MOVE LOW-VALUE           TO W-WDQ501KY-VIPSTILLK-MIN-X               
135600     MOVE HIGH-VALUE          TO W-WDQ501KY-VIPSTILLK-MAX-X               
135700     MOVE WS-IDORDER          TO W-RADB-IDORDER-VIPSTILLK-MIN             
135800                                 W-RADB-IDORDER-VIPSTILLK-MAX             
135900     MOVE RADB-IDARTNR-TILLK  TO W-RADB-IDARTNR-VIPSTILLK-MIN             
136000                                 W-RADB-IDARTNR-VIPSTILLK-MAX             
136100                                                                          
136200     PERFORM IMS-GU-RADB-VIPSTILLK-MIN-MAX                                
136300     PERFORM UNTIL ((VIPSTILLK-RADB-BASEN-SLUT)                           
136400               OR   (VIPSTILLK-RADB-SEGMENT-SAKNAS))                      
136500       IF VIPSTILLK-RADB-FLTILLK = VIPS                                   
136600          IF RADB-KVBEART = VIPSTILLK-RADB-KVBEART                        
136700            MOVE SPACE TO WS-KDORDBEK                                     
136800                                                                          
136900            MOVE LOW-VALUE               TO W-WDQ101KY-MIN-X              
137000            MOVE HIGH-VALUE              TO W-WDQ101KY-MAX-X              
137100            MOVE RADB-IDORDER            TO W-OBKR-IDORDER-MIN            
137200                                            W-OBKR-IDORDER-MAX            
137300            MOVE VIPSTILLK-RADB-IDARTNR  TO W-OBKR-IDARTNR-MIN            
137400                                            W-OBKR-IDARTNR-MAX            
137500            MOVE VIPSTILLK-RADB-IDLOPNR  TO W-OBKR-IDLOPNR-MIN            
137600                                            W-OBKR-IDLOPNR-MAX            
137700            PERFORM IMS-GU-OBKR-MIN-MAX                                   
137800            PERFORM UNTIL ((OBKR-BASEN-SLUT)                              
137900                      OR   (OBKR-SEGMENT-SAKNAS))                         
138000               IF VIPSTILLK-RADB-KVBEART = OBKR-KVBEART                   
138100                IF VIPSTILLK-RADB-IDKUNDRF-RO(3:5) =                      
138200                   OBKR-IDKUNDRF-RO(3:5)                                  
138300                  IF OBKR-IDARTNR-TILLK = ZERO                            
138400                     PERFORM S28-VIPSTILLK-PULSERS-RAD-1-2                
138500                  ELSE                                                    
138600                     IF OBKR-IDDC NOT = WS-IDDC-HOME                      
138700                        PERFORM UNTIL                                     
138800                             ((OBKR-BASEN-SLUT)     OR                    
138900                              (OBKR-SEGMENT-SAKNAS) OR                    
139000                              (OBKR-KDORDBEK = 15 OR 16))                 
139100                             PERFORM IMS-GN-OBKR-MIN-MAX                  
139200                        END-PERFORM                                       
139300                        IF OBKR-KDORDBEK = 15 OR 16                       
139400                           PERFORM S25-REFERAD-PULSERS                    
139500                        END-IF                                            
139600                     ELSE                                                 
139700                        IF OBKR-KDORDBEK = 41                             
139800                           MOVE VIPS TO TILLKOMMANDE-SW                   
139900                           PERFORM                                        
140000                               S27-PULSOBKR-OCH-LEV-TILLK                 
140100                        END-IF                                            
140200                     END-IF                                               
140300                  END-IF                                                  
140400                END-IF                                                    
140500               END-IF                                                     
140600               PERFORM  IMS-GN-OBKR-MIN-MAX                               
140700            END-PERFORM                                                   
140800         END-IF                                                           
140900       END-IF                                                             
141000       PERFORM IMS-GN-RADB-VIPSTILLK-MIN-MAX                              
141100     END-PERFORM                                                          
141200     .                                                                    
141300     EJECT                                                                
141400                                                                          
141500 FC3-VIPSTILLK-PULSOBKR SECTION.                                          
141600                                                                          
141700     MOVE 'STA-FC3 '          TO FELTEXT                                  
141800                                                                          
141900     MOVE LOW-VALUE           TO W-WDQ501KY-VIPSTILLK-MIN-X               
142000     MOVE HIGH-VALUE          TO W-WDQ501KY-VIPSTILLK-MAX-X               
142100     MOVE WS-IDORDER          TO W-RADB-IDORDER-VIPSTILLK-MIN             
142200                                 W-RADB-IDORDER-VIPSTILLK-MAX             
142300     MOVE RADB-IDARTNR-TILLK  TO W-RADB-IDARTNR-VIPSTILLK-MIN             
142400                                 W-RADB-IDARTNR-VIPSTILLK-MAX             
142500                                                                          
142600     MOVE NEJ                 TO TRAFF-SW                                 
142700                                                                          
142800     PERFORM IMS-GU-RADB-VIPSTILLK-MIN-MAX                                
142900     PERFORM UNTIL ((VIPSTILLK-RADB-BASEN-SLUT)                           
143000               OR   (VIPSTILLK-RADB-SEGMENT-SAKNAS)                       
143100               OR   (TRAFF ))                                             
143200        MOVE VIPSTILLK-RADB-IDPURAD TO WS-SPAR-IDPURAD                    
143300        MOVE VIPSTILLK-RADB-IDKOLLI TO WS-SPAR-IDKOLLI                    
143400        PERFORM UNTIL ((VIPSTILLK-RADB-BASEN-SLUT)                        
143500                  OR   (VIPSTILLK-RADB-SEGMENT-SAKNAS)                    
143600                  OR   (TRAFF )                                           
143700                  OR   (VIPSTILLK-RADB-IDPURAD NOT =                      
143800                                       WS-SPAR-IDPURAD))                  
143900           IF VIPSTILLK-RADB-FLTILLK = VIPS                               
144000            IF RADB-KVBEART = VIPSTILLK-RADB-KVBEART                      
144100              IF VIPSTILLK-RADB-IDKOLLI = WS-SPAR-IDKOLLI                 
144200                 MOVE LOW-VALUE          TO                               
144300                                   W-WDQ101KY-LEV-MIN-X                   
144400                 MOVE HIGH-VALUE         TO                               
144500                                   W-WDQ101KY-LEV-MAX-X                   
144600                 MOVE VIPSTILLK-RADB-IDORDER TO                           
144700                                   W-OBKR-IDORDER-LEV-MIN                 
144800                                   W-OBKR-IDORDER-LEV-MAX                 
144900                 MOVE VIPSTILLK-RADB-IDARTNR TO                           
145000                                   W-OBKR-IDARTNR-LEV-MIN                 
145100                                   W-OBKR-IDARTNR-LEV-MAX                 
145200                                                                          
145300                 MOVE VIPSTILLK-RADB-KVBEART TO WS-KVBEART                
145400                 MOVE VIPSTILLK-RADB-KVBEART-Q TO WS-KVBEART-Q            
145500                                                                          
145600                 PERFORM IMS-GU-OBKR-LEV-MIN-MAX                          
145700                 PERFORM UNTIL ((OBKR-LEV-BASEN-SLUT)                     
145800                           OR   (OBKR-LEV-SEGMENT-SAKNAS))                
145900                    IF ((RADB-IDDC = WS-IDDC-HOME    )  AND               
146000                        (RADB-IDDC = WS-IDDC-LEV     )  AND               
146100                        (LEV-OBKR-FLTILLK = NEJ      ))                   
146200                    OR ((WS-IDDC-HOME = WS-IDDC-LEV  )  AND               
146300                        (RADB-IDDC NOT = WS-IDDC-LEV )  AND               
146400                        (LEV-OBKR-KDORDBEK = 15 OR 16)  AND               
146500                        (LEV-OBKR-FLTILLK = NEJ      ))                   
146600                    OR ((RADB-IDDC NOT = WS-IDDC-HOME)  AND               
146700                        (RADB-IDDC     = WS-IDDC-LEV ))                   
146800                       IF WS-KVBEART   = LEV-OBKR-KVBEART                 
146900                       OR WS-KVBEART-Q = LEV-OBKR-KVBEART-Q               
147000                        IF VIPSTILLK-RADB-IDKUNDRF-RO(3:5) =              
147100                           LEV-OBKR-IDKUNDRF-RO(3:5)                      
147200                          INITIALIZE DN-DETALJRAD                         
147300                          PERFORM S26B-FL-VIPSTILLK-RADB-UPPG             
147400                          MOVE VIPS TO TILLKOMMANDE-SW                    
147500                          PERFORM S24-PULSORDERBEKR                       
147600                          IF SKRIV-LEVRAD                                 
147700                             IF WS-KDORDBEK NOT = SPACE                   
147800                                MOVE WS-KDORDBEK  TO                      
147900                                          RED-KDORDBEK-KOD                
148000                                MOVE '*'          TO                      
148100                                          RED-KDORDBEK-X                  
148200                                MOVE RED-KDORDBEK TO                      
148300                                          DN-RAD-KDORDBEK                 
148400                             ELSE                                         
148500                                MOVE '*'          TO                      
148600                                          DN-RAD-KDORDBEK                 
148700                             END-IF                                       
148800                             IF VIPSTILLK-RADB-KVLEVART >                 
148900                                                ZERO AND                  
149000                                VIPSTILLK-RADB-IDDC = WS-IDDC-LEV         
149100                                COMPUTE WS-SUM-DELIV-LINES =              
149200                                        WS-SUM-DELIV-LINES + 1            
149300                             END-IF                                       
149400                                                                          
149500                             MOVE JA    TO TRAFF-SW                       
149600                             PERFORM S42-SKRIV-DETALJRAD                  
149700                          END-IF                                          
149800                        END-IF                                            
149900                       END-IF                                             
150000                    END-IF                                                
150100                    PERFORM IMS-GN-OBKR-LEV-MIN-MAX                       
150200                 END-PERFORM                                              
150300              ELSE                                                        
150400                 INITIALIZE DN-DETALJRAD                                  
150500                 PERFORM S22B-FLYTTA-KOLLIRAD-VIPSTILLK                   
150600                 MOVE '*'        TO DN-RAD-KDORDBEK                       
150700                 PERFORM S42-SKRIV-DETALJRAD                              
150800              END-IF                                                      
150900              IF SPARRTEXT                                                
151000                 PERFORM S29-SPARRTEXT                                    
151100              END-IF                                                      
151200            END-IF                                                        
151300           END-IF                                                         
151400           PERFORM IMS-GN-RADB-VIPSTILLK-MIN-MAX                          
151500        END-PERFORM                                                       
151600     END-PERFORM                                                          
151700     .                                                                    
151800     EJECT                                                                
151900                                                                          
152000 FC4-VIPSTILLK-LEVRAD SECTION.                                            
152100                                                                          
152200     MOVE 'STA-FC4 '      TO FELTEXT                                      
152300                                                                          
152400     MOVE LOW-VALUE           TO W-WDQ501KY-VIPSTILLK-MIN-X               
152500     MOVE HIGH-VALUE          TO W-WDQ501KY-VIPSTILLK-MAX-X               
152600     MOVE WS-IDORDER          TO W-RADB-IDORDER-VIPSTILLK-MIN             
152700                                 W-RADB-IDORDER-VIPSTILLK-MAX             
152800     MOVE RADB-IDARTNR-TILLK  TO W-RADB-IDARTNR-VIPSTILLK-MIN             
152900                                 W-RADB-IDARTNR-VIPSTILLK-MAX             
153000                                                                          
153100     INITIALIZE DN-DETALJRAD                                              
153200     MOVE NEJ TO TRAFF-SW                                                 
153300     PERFORM IMS-GU-RADB-VIPSTILLK-MIN-MAX                                
153400     IF SEGMENT-FINNS                                                     
153500        PERFORM UNTIL ((VIPSTILLK-RADB-BASEN-SLUT)                        
153600                  OR   (VIPSTILLK-RADB-SEGMENT-SAKNAS)                    
153700                  OR   (TRAFF))                                           
153800           MOVE VIPSTILLK-RADB-IDPURAD TO WS-SPAR-IDPURAD                 
153900           MOVE VIPSTILLK-RADB-IDKOLLI TO WS-SPAR-IDKOLLI                 
154000           PERFORM UNTIL ((VIPSTILLK-RADB-BASEN-SLUT)                     
154100                     OR   (VIPSTILLK-RADB-SEGMENT-SAKNAS)                 
154200                     OR   (TRAFF)                                         
154300                     OR   (VIPSTILLK-RADB-IDPURAD NOT =                   
154400                                          WS-SPAR-IDPURAD))               
154500              IF VIPSTILLK-RADB-FLTILLK = VIPS                            
154600                 IF RADB-KVBEART = VIPSTILLK-RADB-KVBEART                 
154700                    IF VIPSTILLK-RADB-IDKOLLI = WS-SPAR-IDKOLLI           
154800                       INITIALIZE DN-DETALJRAD                            
154900                       PERFORM S26B-FL-VIPSTILLK-RADB-UPPG                
155000                       MOVE '*'        TO DN-RAD-KDORDBEK                 
155100                       MOVE JA         TO TRAFF-SW                        
155200                       PERFORM S42-SKRIV-DETALJRAD                        
155300                       IF VIPSTILLK-RADB-KVLEVART > ZERO AND              
155400                          VIPSTILLK-RADB-IDDC = WS-IDDC-LEV               
155500                          COMPUTE WS-SUM-DELIV-LINES =                    
155600                                  WS-SUM-DELIV-LINES + 1                  
155700                       END-IF                                             
155800                    ELSE                                                  
155900                       INITIALIZE DN-DETALJRAD                            
156000                       PERFORM S22B-FLYTTA-KOLLIRAD-VIPSTILLK             
156100                       MOVE '*'        TO DN-RAD-KDORDBEK                 
156200                       PERFORM S42-SKRIV-DETALJRAD                        
156300                    END-IF                                                
156400                    IF VIPSTILLK-RADB-FLDIRLEV = JA                       
156500                       INITIALIZE DN-DETALJRAD                            
156600                       MOVE 'GNB DELIV.'   TO DN-RAD-BEART-USA            
156700                       PERFORM S42-SKRIV-DETALJRAD                        
156800                    END-IF                                                
156900                 END-IF                                                   
157000              END-IF                                                      
157100              PERFORM IMS-GN-RADB-VIPSTILLK-MIN-MAX                       
157200           END-PERFORM                                                    
157300        END-PERFORM                                                       
157400     ELSE                                                                 
157500*      OM DEN TILLKOMMANDE HAR ANNULLERATS I VIPS HAR ERSÄTT-             
157600*      NINGSINFON REDAN SKRIVITS, MEN ORDERRADEN FINNS EJ                 
157700        MOVE RADB-IDARTNR-TILLK        TO DN-RAD-IDARTNR                  
157800        MOVE '-'                       TO DN-RAD-STRECK                   
157900        MOVE RADB-IDARTNR-TILLK        TO KSIF-FLT                        
158000        PERFORM S52-BERAKNA-REKSIFFR                                      
158100        MOVE KSIF-KSIFF                TO DN-RAD-REKSIFFR                 
158200        MOVE RADB-IDARTNR-TILLK        TO W-BENA-IDARTNR                  
158300        PERFORM S51-HAMTA-BENAMNING                                       
158400        MOVE WS-BEART-USA              TO DN-RAD-BEART-USA                
158500        MOVE '83*'                     TO DN-RAD-KDORDBEK                 
158600        PERFORM S42-SKRIV-DETALJRAD                                       
158700     END-IF                                                               
158800     .                                                                    
158900     EJECT                                                                
159000                                                                          
159100 FD-KOLLA-PULS-OBKR SECTION.                                              
159200                                                                          
159300     MOVE 'STA-FD  '      TO FELTEXT                                      
159400                                                                          
159500     MOVE NEJ          TO PULS-ERS-SW                                     
159600                          PULS-OBKR-SW                                    
159700                          TRAFF-SW                                        
159800     MOVE LOW-VALUE    TO W-WDQ101KY-MIN-X                                
159900     MOVE HIGH-VALUE   TO W-WDQ101KY-MAX-X                                
160000     MOVE RADB-IDORDER TO W-OBKR-IDORDER-MIN                              
160100                          W-OBKR-IDORDER-MAX                              
160200     MOVE RADB-IDARTNR TO W-OBKR-IDARTNR-MIN                              
160300                          W-OBKR-IDARTNR-MAX                              
160400     PERFORM IMS-GU-OBKR-MIN-MAX                                          
160500     PERFORM UNTIL ((OBKR-SEGMENT-SAKNAS)                                 
160600               OR   (OBKR-BASEN-SLUT)                                     
160700               OR   (TRAFF))                                              
160800        IF RADB-KVBEART = OBKR-KVBEART                                    
160900        OR RADB-KVBEART-Q = OBKR-KVBEART-Q                                
161000         IF RADB-IDKUNDRF-RO(3:5) = OBKR-IDKUNDRF-RO(3:5)                 
161100           IF OBKR-KDORDBEK = 41                                          
161200              IF RADB-IDDC = WS-IDDC-LEV                                  
161300                 MOVE JA TO PULS-ERS-SW                                   
161400              END-IF                                                      
161500           ELSE                                                           
161600              IF OBKR-KDORDBEK > ZERO                                     
161700                 IF ((OBKR-KDORDBEK = 15 OR 16)                           
161800                 AND (RADB-IDDC NOT = OBKR-IDDC))                         
161900                    CONTINUE                                              
162000                 ELSE                                                     
162100                    MOVE JA TO PULS-OBKR-SW                               
162200                 END-IF                                                   
162300              END-IF                                                      
162400           END-IF                                                         
162500         END-IF                                                           
162600        END-IF                                                            
162700        PERFORM IMS-GN-OBKR-MIN-MAX                                       
162800     END-PERFORM                                                          
162900     .                                                                    
163000     EJECT                                                                
163100                                                                          
163200 FF-PULSERSATTNING SECTION.                                               
163300                                                                          
163400     MOVE 'STA-FF  '      TO FELTEXT                                      
163500                                                                          
163600     MOVE SPACE TO WS-KDORDBEK                                            
163700                                                                          
163800     MOVE LOW-VALUE               TO W-WDQ101KY-MIN-X                     
163900     MOVE HIGH-VALUE              TO W-WDQ101KY-MAX-X                     
164000     MOVE RADB-IDORDER            TO W-OBKR-IDORDER-MIN                   
164100                                     W-OBKR-IDORDER-MAX                   
164200     MOVE RADB-IDARTNR            TO W-OBKR-IDARTNR-MIN                   
164300                                     W-OBKR-IDARTNR-MAX                   
164400     MOVE RADB-IDLOPNR            TO W-OBKR-IDLOPNR-MIN                   
164500                                     W-OBKR-IDLOPNR-MAX                   
164600     PERFORM IMS-GU-OBKR-MIN-MAX                                          
164700     PERFORM UNTIL ((OBKR-BASEN-SLUT)                                     
164800               OR   (OBKR-SEGMENT-SAKNAS))                                
164900        IF RADB-IDKUNDRF-RO(3:5) = OBKR-IDKUNDRF-RO(3:5)                  
165000           IF OBKR-IDARTNR-TILLK = ZERO                                   
165100              PERFORM S23-PULSERS-RAD-1-2                                 
165200           ELSE                                                           
165300              IF OBKR-IDDC NOT = WS-IDDC-HOME                             
165400                 PERFORM UNTIL ((BASEN-SLUT)     OR                       
165500                                (SEGMENT-SAKNAS) OR                       
165600                                (OBKR-KDORDBEK = 15 OR 16))               
165700                    PERFORM IMS-GN-OBKR-MIN-MAX                           
165800                 END-PERFORM                                              
165900                 IF OBKR-KDORDBEK = 15 OR 16                              
166000                    PERFORM S25-REFERAD-PULSERS                           
166100                 END-IF                                                   
166200              ELSE                                                        
166300                 IF OBKR-KDORDBEK = 41                                    
166400                    MOVE PULS TO TILLKOMMANDE-SW                          
166500                    PERFORM S27-PULSOBKR-OCH-LEV-TILLK                    
166600                 END-IF                                                   
166700              END-IF                                                      
166800           END-IF                                                         
166900        END-IF                                                            
167000        PERFORM IMS-GN-OBKR-MIN-MAX                                       
167100     END-PERFORM                                                          
167200     .                                                                    
167300     EJECT                                                                
167400                                                                          
167500 FG-PULSOBKR SECTION.                                                     
167600                                                                          
167700     MOVE 'STA-FG  '      TO FELTEXT                                      
167800                                                                          
167900     MOVE LOW-VALUE    TO W-WDQ101KY-LEV-MIN-X                            
168000     MOVE HIGH-VALUE   TO W-WDQ101KY-LEV-MAX-X                            
168100     MOVE RADB-IDORDER TO W-OBKR-IDORDER-LEV-MIN                          
168200                          W-OBKR-IDORDER-LEV-MAX                          
168300     MOVE RADB-IDARTNR TO W-OBKR-IDARTNR-LEV-MIN                          
168400                          W-OBKR-IDARTNR-LEV-MAX                          
168500                                                                          
168600     MOVE RADB-KVBEART   TO WS-KVBEART                                    
168700     MOVE RADB-KVBEART-Q TO WS-KVBEART-Q                                  
168800                                                                          
168900     PERFORM IMS-GU-OBKR-LEV-MIN-MAX                                      
169000     PERFORM UNTIL ((OBKR-LEV-BASEN-SLUT)                                 
169100               OR   (OBKR-LEV-SEGMENT-SAKNAS))                            
169200        IF ((RADB-IDDC = WS-IDDC-HOME    )  AND                           
169300            (RADB-IDDC = WS-IDDC-LEV     )  AND                           
169400            ((LEV-OBKR-FLTILLK = NEJ     )                                
169500              OR                                                          
169600             ((LEV-OBKR-FLTILLK = JA) AND                                 
169700              (LEV-OBKR-IDKUNDRF-RO(3:5)                                  
169800                 NOT = '00000'           ))))                             
169900                                                                          
170000        OR ((WS-IDDC-HOME = WS-IDDC-LEV  )  AND                           
170100            (RADB-IDDC NOT = WS-IDDC-LEV )  AND                           
170200            (LEV-OBKR-KDORDBEK = 15 OR 16)  AND                           
170300            ((LEV-OBKR-FLTILLK = NEJ     )                                
170400              OR                                                          
170500             ((LEV-OBKR-FLTILLK = JA) AND                                 
170600              (LEV-OBKR-IDKUNDRF-RO(3:5)                                  
170700                 NOT = '00000'           ))))                             
170800                                                                          
170900        OR ((RADB-IDDC NOT = WS-IDDC-HOME)  AND                           
171000            (RADB-IDDC     = WS-IDDC-LEV ))                               
171100           IF WS-KVBEART   = LEV-OBKR-KVBEART                             
171200           OR WS-KVBEART-Q = LEV-OBKR-KVBEART-Q                           
171300            IF RADB-IDKUNDRF-RO(3:5) = LEV-OBKR-IDKUNDRF-RO(3:5)          
171400              INITIALIZE DN-DETALJRAD                                     
171500              PERFORM S20-FLYTTA-RADB-UPPG                                
171600              MOVE NEJ TO TILLKOMMANDE-SW                                 
171700              PERFORM S24-PULSORDERBEKR                                   
171800              IF SKRIV-LEVRAD                                             
171900                 MOVE WS-KDORDBEK TO DN-RAD-KDORDBEK                      
172000                 IF RADB-KVLEVART > ZERO AND                              
172100                    RADB-IDDC = WS-IDDC-LEV                               
172200                    COMPUTE WS-SUM-DELIV-LINES =                          
172300                            WS-SUM-DELIV-LINES + 1                        
172400                 END-IF                                                   
172500                 PERFORM S42-SKRIV-DETALJRAD                              
172600                 IF SPARRTEXT                                             
172700                    PERFORM S29-SPARRTEXT                                 
172800                 END-IF                                                   
172900              END-IF                                                      
173000            END-IF                                                        
173100           END-IF                                                         
173200        END-IF                                                            
173300        PERFORM IMS-GN-OBKR-LEV-MIN-MAX                                   
173400     END-PERFORM                                                          
173500     .                                                                    
173600     EJECT                                                                
173700                                                                          
173800 FH-LEVRAD SECTION.                                                       
173900                                                                          
174000     MOVE 'STA-FH  '      TO FELTEXT                                      
174100                                                                          
174200     IF RADB-IDDC = WS-IDDC-LEV                                           
174300        INITIALIZE DN-DETALJRAD                                           
174400        PERFORM S20-FLYTTA-RADB-UPPG                                      
174500                                                                          
174600        IF RADB-KVLEVART > ZERO AND                                       
174700           RADB-IDDC = WS-IDDC-LEV                                        
174800           COMPUTE WS-SUM-DELIV-LINES =                                   
174900                   WS-SUM-DELIV-LINES + 1                                 
175000        END-IF                                                            
175100        PERFORM S42-SKRIV-DETALJRAD                                       
175200                                                                          
175300        IF RADB-FLDIRLEV = JA                                             
175400           INITIALIZE DN-DETALJRAD                                        
175500           MOVE 'GNB DELIV.'             TO DN-RAD-BEART-USA              
175600           PERFORM S42-SKRIV-DETALJRAD                                    
175700        END-IF                                                            
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100                                                                          
176200 FI-EXTRA-KOLLI-RAD SECTION.                                              
176300                                                                          
176400     MOVE 'STA-FI  '      TO FELTEXT                                      
176500                                                                          
176600     IF RADB-IDDC = WS-IDDC-LEV                                           
176700        INITIALIZE DN-DETALJRAD                                           
176800        PERFORM S21-FLYTTA-KOLLIRAD                                       
176900        PERFORM S42-SKRIV-DETALJRAD                                       
177000     END-IF                                                               
177100     .                                                                    
177200     EJECT                                                                
177300                                                                          
177400 G-DN-SLUTRADER SECTION.                                                  
177500                                                                          
177600     MOVE 'STA-G   '      TO FELTEXT                                      
177700                                                                          
177800     MOVE 'GR WEIGHT:'             TO DN-SRAD1-GR-WEIGHT                  
177900     MOVE 'GR CUBE:'               TO DN-SRAD2-GR-CUBE                    
178000     MOVE 'NO OF DELIV LINES:'     TO DN-SRAD3-LINES                      
178100     MOVE 'NO OF CASES:'           TO DN-SRAD4-CASES                      
178200                                                                          
178300     IF ORDERDEL-UTSKRIVEN                                                
178400        IF DCS-NDC-NA AND DCS-USA                                         
178500           MOVE 'POUNDS:'                TO DN-SRAD1-VIKT                 
178600           MOVE 'FT3:'                   TO DN-SRAD2-KUBIK                
178700           COMPUTE WS-VKORDBTO  ROUNDED =                                 
178800                   VORD-VKORDBTO * CONV-KG-TO-LB                          
178900           END-COMPUTE                                                    
179000                                                                          
179100           COMPUTE WS-VLORDBTO  ROUNDED =                                 
179200                   VORD-VLORDBTO * CONV-M3-TO-FT3                         
179300           END-COMPUTE                                                    
179400        END-IF                                                            
179500        IF DCS-NDC-NA AND DCS-CANADA                                      
179600           MOVE 'KG    :'             TO DN-SRAD1-VIKT                    
179700           MOVE 'M3 :'                TO DN-SRAD2-KUBIK                   
179800           MOVE VORD-VKORDBTO         TO WS-VKORDBTO                      
179900           MOVE VORD-VLORDBTO         TO WS-VLORDBTO                      
180000        END-IF                                                            
180100        MOVE WS-VKORDBTO              TO DN-SRAD1-VKORDBTO                
180200        MOVE WS-VLORDBTO              TO DN-SRAD2-VLORDBTO                
180300        MOVE WS-SUM-DELIV-LINES       TO DN-SRAD3-SUM-LINES               
180400        MOVE VORD-KVKOLPAC            TO DN-SRAD4-SUM-CASES               
180500     END-IF                                                               
180600     IF W-IDTRANS = 4341                                                  
180700        MOVE 'THIS DELIVERY NOTE IS PRINTED FROM SCREEN 4341'             
180800                                  TO DN-ERROR-TEXT                        
180900     END-IF                                                               
181000                                                                          
181100     IF WS-RADRAK > 58                                                    
181200        PERFORM S41-SKRIV-RUBRIKER                                        
181300     END-IF                                                               
181400     PERFORM S43-SKRIV-SLUTRADER                                          
181500     .                                                                    
181600     EJECT                                                                
181700                                                                          
181800 H-AVSLUTA-LISTA SECTION.                                                 
181900                                                                          
182000     MOVE 'STA-H   '      TO FELTEXT                                      
182100                                                                          
182200     MOVE MID-IDDC        TO WS-IDDC                                      
182300     IF GMT-FLDNDAP = NEJ AND NDC-NA                                      
182400        CONTINUE                                                          
182500     ELSE                                                                 
182600        IF GMT-FLDNDAP = JA                                               
182700          AND (HUV-RADB-KDORDKL = 0 OR 1 OR 2 OR 3 OR 4)                  
182800                                                                          
182900          MOVE BLANKRAD              TO SEND-RAD                          
183000          MOVE WS-SKIP1              TO STYRTECKEN-RAD                    
183100          MOVE LENGTH OF BLANKRAD    TO SEND-KVDLEN                       
183200          PERFORM S90-SEND-CLOSE                                          
183300        ELSE                                                              
183400          CALL W006PRS1 USING PRT-SPOOL-A4S                               
183500                              PRT-CLOSE                                   
183600                              WS-VALD-PRINTER                             
183700                              ALT-PCB                                     
183800                              WS-DUMMY                                    
183900                              PRT-IDLIST                                  
184000        END-IF                                                            
184100     END-IF                                                               
184200     .                                                                    
184300     EJECT                                                                
184400                                                                          
184500 S20-FLYTTA-RADB-UPPG SECTION.                                            
184600                                                                          
184700     MOVE 'STA-S20 '      TO FELTEXT                                      
184800                                                                          
184900     MOVE RADB-IDARTNR             TO DN-RAD-IDARTNR                      
185000     MOVE '-'                      TO DN-RAD-STRECK                       
185100     MOVE RADB-REKSIFFR            TO DN-RAD-REKSIFFR                     
185200     MOVE RADB-BEART-USA           TO DN-RAD-BEART-USA                    
185300     MOVE RADB-KVLEVART            TO DN-RAD-KVLEVART                     
185400     MOVE RADB-KVBEART             TO DN-RAD-KVBEART                      
185500     MOVE RADB-IDKOLLI             TO DN-RAD-IDKOLLI                      
185600     MOVE RADB-BERADREF            TO DN-RAD-BERADREF                     
185700     MOVE RADB-IDKUNDRF-RO(3:5)    TO DN-RAD-IDKUNDRF-RO                  
185800     .                                                                    
185900     EJECT                                                                
186000                                                                          
186100 S21-FLYTTA-KOLLIRAD SECTION.                                             
186200                                                                          
186300     MOVE 'STA-S21 '      TO FELTEXT                                      
186400                                                                          
186500     MOVE RADB-IDARTNR             TO DN-RAD-IDARTNR                      
186600     MOVE '-'                      TO DN-RAD-STRECK                       
186700     MOVE RADB-REKSIFFR            TO DN-RAD-REKSIFFR                     
186800     MOVE RADB-BEART-USA           TO DN-RAD-BEART-USA                    
186900     MOVE RADB-KVLEVART            TO DN-RAD-KVLEVART                     
187000     MOVE RADB-IDKOLLI             TO DN-RAD-IDKOLLI                      
187100     MOVE RADB-BERADREF            TO DN-RAD-BERADREF                     
187200     MOVE RADB-IDKUNDRF-RO(3:5)    TO DN-RAD-IDKUNDRF-RO                  
187300     .                                                                    
187400     EJECT                                                                
187500                                                                          
187600 S22-FLYTTA-KOLLIRAD-TILLK SECTION.                                       
187700                                                                          
187800     MOVE 'STA-S22 '      TO FELTEXT                                      
187900                                                                          
188000     MOVE TILLK-RADB-IDARTNR       TO DN-RAD-IDARTNR                      
188100     MOVE '-'                      TO DN-RAD-STRECK                       
188200     MOVE TILLK-RADB-REKSIFFR      TO DN-RAD-REKSIFFR                     
188300     MOVE TILLK-RADB-BEART-USA     TO DN-RAD-BEART-USA                    
188400     MOVE TILLK-RADB-KVLEVART      TO DN-RAD-KVLEVART                     
188500     MOVE TILLK-RADB-IDKOLLI       TO DN-RAD-IDKOLLI                      
188600     MOVE TILLK-RADB-BERADREF      TO DN-RAD-BERADREF                     
188700     MOVE TILLK-RADB-IDKUNDRF-RO(3:5) TO DN-RAD-IDKUNDRF-RO               
188800     .                                                                    
188900     EJECT                                                                
189000                                                                          
189100 S22B-FLYTTA-KOLLIRAD-VIPSTILLK SECTION.                                  
189200                                                                          
189300     MOVE 'STA-S22 '      TO FELTEXT                                      
189400                                                                          
189500     MOVE VIPSTILLK-RADB-IDARTNR   TO DN-RAD-IDARTNR                      
189600     MOVE '-'                      TO DN-RAD-STRECK                       
189700     MOVE VIPSTILLK-RADB-REKSIFFR  TO DN-RAD-REKSIFFR                     
189800     MOVE VIPSTILLK-RADB-BEART-USA TO DN-RAD-BEART-USA                    
189900     MOVE VIPSTILLK-RADB-KVLEVART  TO DN-RAD-KVLEVART                     
190000     MOVE VIPSTILLK-RADB-IDKOLLI   TO DN-RAD-IDKOLLI                      
190100     MOVE VIPSTILLK-RADB-BERADREF  TO DN-RAD-BERADREF                     
190200     MOVE VIPSTILLK-RADB-IDKUNDRF-RO(3:5) TO DN-RAD-IDKUNDRF-RO           
190300     .                                                                    
190400     EJECT                                                                
190500                                                                          
190600 S23-PULSERS-RAD-1-2 SECTION.                                             
190700                                                                          
190800     INITIALIZE DN-DETALJRAD                                              
190900                                                                          
191000     MOVE RADB-IDARTNR          TO DN-RAD-IDARTNR                         
191100     MOVE '-'                   TO DN-RAD-STRECK                          
191200     MOVE RADB-REKSIFFR         TO DN-RAD-REKSIFFR                        
191300     MOVE OBKR-KVBEART          TO DN-RAD-KVBEART                         
191400     MOVE OBKR-KDORDBEK         TO RED-KDORDBEK-KOD                       
191500     MOVE '*'                   TO RED-KDORDBEK-X                         
191600     MOVE RED-KDORDBEK          TO DN-RAD-KDORDBEK                        
191700     MOVE RADB-BEART-USA        TO DN-RAD-BEART-USA                       
191800     PERFORM S42-SKRIV-DETALJRAD                                          
191900     IF OBKR-KDORDBEK = 41                                                
192000        INITIALIZE DN-DETALJRAD                                           
192100        MOVE 'REPLACED BY'      TO DN-RAD-BEART-USA                       
192200        MOVE '*'                TO DN-RAD-KDORDBEK                        
192300        PERFORM S42-SKRIV-DETALJRAD                                       
192400     END-IF                                                               
192500     .                                                                    
192600     EJECT                                                                
192700                                                                          
192800 S24-PULSORDERBEKR SECTION.                                               
192900                                                                          
193000     MOVE 'STA-S24 '      TO FELTEXT                                      
193100                                                                          
193200     MOVE JA              TO LEVRAD-SW                                    
193300     MOVE SPACE           TO WS-KDORDBEK                                  
193400                             DN-RAD-IDDC                                  
193500                                                                          
193600     EVALUATE TRUE                                                        
193700        WHEN LEV-OBKR-KDORDBEK =       90 OR 91 OR 92 OR 93 OR 98         
193800             PERFORM S24B-VISA-RO-UPPG                                    
193900        WHEN LEV-OBKR-KDORDBEK =       10 OR 51 OR 52 OR 54 OR            
194000                                       55 OR 57 OR 58 OR 67 OR 83         
194100             PERFORM S24C-VISA-KOD-OCH-RO                                 
194200        WHEN LEV-OBKR-KDORDBEK =       15 OR 16                           
194300             PERFORM S24D-VISA-REFERAL                                    
194400        WHEN LEV-OBKR-KDORDBEK =       61                                 
194500             PERFORM S24F-VARIABEL                                        
194600        WHEN LEV-OBKR-KDORDBEK =       99                                 
194700             PERFORM S24G-PREL-RO                                         
194800        WHEN LEV-OBKR-KDORDBEK =       43 OR 44                           
194900             PERFORM S24H-KVANTANPASSNING                                 
195000        WHEN LEV-OBKR-KDORDBEK =             85                           
195100             PERFORM S24E-INGEN-VISNING                                   
195200                                                                          
195300     END-EVALUATE                                                         
195400     .                                                                    
195500     EJECT                                                                
195600                                                                          
195700 S24B-VISA-RO-UPPG SECTION.                                               
195800                                                                          
195900     MOVE 'STA-S24B'      TO FELTEXT                                      
196000                                                                          
196100     IF LEV-OBKR-IDDC = WS-IDDC-LEV                                       
196200        IF LEV-OBKR-KVBEART-Q = WS-KVBEART-Q                              
196300           MOVE LEV-OBKR-KVRO        TO DN-RAD-KVRO                       
196400           IF LEV-OBKR-KDORDBEK = 90 OR 91 OR 98                          
196500              MOVE JA TO SPARRTEXT-SW                                     
196600           END-IF                                                         
196700        END-IF                                                            
196800     END-IF                                                               
196900     .                                                                    
197000     EJECT                                                                
197100                                                                          
197200 S24C-VISA-KOD-OCH-RO SECTION.                                            
197300                                                                          
197400     MOVE 'STA-S24C'      TO FELTEXT                                      
197500                                                                          
197600     IF LEV-OBKR-IDDC = WS-IDDC-LEV                                       
197700        IF LEV-OBKR-KVBEART = WS-KVBEART                                  
197800           MOVE LEV-OBKR-KDORDBEK    TO WS-KDORDBEK                       
197900           MOVE LEV-OBKR-KVRO        TO DN-RAD-KVRO                       
198000        END-IF                                                            
198100     END-IF                                                               
198200     .                                                                    
198300     EJECT                                                                
198400                                                                          
198500 S24D-VISA-REFERAL SECTION.                                               
198600                                                                          
198700     MOVE 'STA-S24D'      TO FELTEXT                                      
198800                                                                          
198900     IF WS-KVBEART = LEV-OBKR-KVBEART                                     
199000        IF LEV-OBKR-IDDC NOT = WS-IDDC-LEV                                
199100           MOVE LEV-OBKR-KDORDBEK    TO WS-KDORDBEK                       
199200           MOVE LEV-OBKR-IDDC        TO DN-RAD-IDDC                       
199300           MOVE ZERO                 TO DN-RAD-KVLEVART                   
199400                                        DN-RAD-IDKOLLI                    
199500        ELSE                                                              
199600           IF VIPSTILLKOMMANDE                                            
199700              IF VIPSTILLK-RADB-KVBEART-Q NOT =                           
199800                                 VIPSTILLK-RADB-KVLEVART-TOT              
199900                 MOVE NEJ                  TO LEVRAD-SW                   
200000              ELSE                                                        
200100                 MOVE JA                   TO LEVRAD-SW                   
200200              END-IF                                                      
200300           ELSE                                                           
200400              IF PULSTILLKOMMANDE                                         
200500                 IF TILLK-RADB-KVBEART-Q NOT =                            
200600                                 TILLK-RADB-KVLEVART-TOT                  
200700                    MOVE NEJ               TO LEVRAD-SW                   
200800                 ELSE                                                     
200900                    MOVE JA                TO LEVRAD-SW                   
201000                 END-IF                                                   
201100              ELSE                                                        
201200                 IF RADB-KVBEART-Q NOT = RADB-KVLEVART-TOT                
201300                    MOVE NEJ               TO LEVRAD-SW                   
201400                 ELSE                                                     
201500                    MOVE JA                TO LEVRAD-SW                   
201600                 END-IF                                                   
201700              END-IF                                                      
201800           END-IF                                                         
201900        END-IF                                                            
202000     END-IF                                                               
202100     .                                                                    
202200     EJECT                                                                
202300                                                                          
202400 S24E-INGEN-VISNING SECTION.                                              
202500                                                                          
202600     MOVE 'STA-S24E'      TO FELTEXT                                      
202700                                                                          
202800     CONTINUE                                                             
202900     MOVE NEJ                      TO LEVRAD-SW                           
203000     .                                                                    
203100     EJECT                                                                
203200                                                                          
203300 S24F-VARIABEL SECTION.                                                   
203400                                                                          
203500     MOVE 'STA-S24F'               TO FELTEXT                             
203600     IF LEV-OBKR-IDDC = WS-IDDC-LEV                                       
203700        IF LEV-OBKR-IDLOPNR = +1 AND LEV-OBKR-IDSEKVNR = +1               
203800           MOVE LEV-OBKR-KDORDBEK     TO DN-RAD-KDORDBEK                  
203900           PERFORM S42-SKRIV-DETALJRAD                                    
204000                                                                          
204100           INITIALIZE DN-DETALJRAD                                        
204200           MOVE 'VARIABLE   '         TO DN-RAD-BEART-USA                 
204300        ELSE                                                              
204400           MOVE NEJ                   TO LEVRAD-SW                        
204500        END-IF                                                            
204600     END-IF                                                               
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 S24G-PREL-RO SECTION.                                                    
205100                                                                          
205200     MOVE 'STA-S24G'               TO FELTEXT                             
205300                                                                          
205400*    OM RADB-KVBEART-Q INTE = RADB-KVLEVART                               
205500*    KOMMER NÄSTA ORDERBEKRÄFTELSE ATT VARA EN 90                         
205600                                                                          
205700     IF VIPSTILLKOMMANDE                                                  
205800        IF VIPSTILLK-RADB-KVBEART-Q NOT =                                 
205900                               VIPSTILLK-RADB-KVLEVART-TOT                
206000           MOVE NEJ                      TO LEVRAD-SW                     
206100        ELSE                                                              
206200           MOVE JA                       TO LEVRAD-SW                     
206300        END-IF                                                            
206400     ELSE                                                                 
206500        IF PULSTILLKOMMANDE                                               
206600           IF TILLK-RADB-KVBEART-Q NOT =                                  
206700                               TILLK-RADB-KVLEVART-TOT                    
206800              MOVE NEJ                   TO LEVRAD-SW                     
206900           ELSE                                                           
207000              MOVE JA                    TO LEVRAD-SW                     
207100           END-IF                                                         
207200        ELSE                                                              
207300           IF RADB-KVBEART-Q NOT = RADB-KVLEVART-TOT                      
207400              MOVE NEJ                   TO LEVRAD-SW                     
207500           ELSE                                                           
207600              MOVE JA                    TO LEVRAD-SW                     
207700           END-IF                                                         
207800        END-IF                                                            
207900     END-IF                                                               
208000     .                                                                    
208100     EJECT                                                                
208200                                                                          
208300 S24H-KVANTANPASSNING SECTION.                                            
208400                                                                          
208500     IF VIPSTILLKOMMANDE                                                  
208600        IF VIPSTILLK-RADB-KVBEART-Q NOT =                                 
208700                                 VIPSTILLK-RADB-KVLEVART                  
208800           MOVE NEJ                      TO LEVRAD-SW                     
208900        ELSE                                                              
209000           MOVE JA                       TO LEVRAD-SW                     
209100        END-IF                                                            
209200     ELSE                                                                 
209300        IF PULSTILLKOMMANDE                                               
209400           IF TILLK-RADB-KVBEART-Q NOT = TILLK-RADB-KVLEVART              
209500              MOVE NEJ                   TO LEVRAD-SW                     
209600           ELSE                                                           
209700              MOVE JA                    TO LEVRAD-SW                     
209800           END-IF                                                         
209900        ELSE                                                              
210000           IF RADB-KVBEART-Q NOT = RADB-KVLEVART                          
210100              IF NDC-US-SE OR NDC-US-CH                                   
210200                MOVE JA                  TO LEVRAD-SW                     
210300              ELSE                                                        
210400                MOVE NEJ                 TO LEVRAD-SW                     
210500              END-IF                                                      
210600           ELSE                                                           
210700              MOVE JA                    TO LEVRAD-SW                     
210800           END-IF                                                         
210900        END-IF                                                            
211000     END-IF                                                               
211100     .                                                                    
211200     EJECT                                                                
211300                                                                          
211400                                                                          
211500 S25-REFERAD-PULSERS SECTION.                                             
211600                                                                          
211700     INITIALIZE DN-DETALJRAD                                              
211800                                                                          
211900     MOVE OBKR-IDARTNR-TILLK     TO DN-RAD-IDARTNR                        
212000     MOVE '-'                    TO DN-RAD-STRECK                         
212100     MOVE OBKR-REKSIFFR-TILLK    TO DN-RAD-REKSIFFR                       
212200     MOVE OBKR-KVBEART           TO DN-RAD-KVBEART                        
212300     MOVE OBKR-IDARTNR-TILLK     TO W-BENA-IDARTNR                        
212400     PERFORM S51-HAMTA-BENAMNING                                          
212500     MOVE WS-BEART-USA           TO DN-RAD-BEART-USA                      
212600     MOVE OBKR-KDORDBEK          TO RED-KDORDBEK-KOD                      
212700     MOVE '*'                    TO RED-KDORDBEK-X                        
212800     MOVE RED-KDORDBEK           TO DN-RAD-KDORDBEK                       
212900     MOVE OBKR-IDDC              TO DN-RAD-IDDC                           
213000     PERFORM S42-SKRIV-DETALJRAD                                          
213100     .                                                                    
213200     EJECT                                                                
213300                                                                          
213400 S26-FLYTTA-TILLK-RADB-UPPG SECTION.                                      
213500                                                                          
213600     MOVE 'STA-S26 '      TO FELTEXT                                      
213700                                                                          
213800     MOVE TILLK-RADB-IDARTNR       TO DN-RAD-IDARTNR                      
213900     MOVE '-'                      TO DN-RAD-STRECK                       
214000     MOVE TILLK-RADB-REKSIFFR      TO DN-RAD-REKSIFFR                     
214100     MOVE TILLK-RADB-BEART-USA     TO DN-RAD-BEART-USA                    
214200     MOVE TILLK-RADB-KVBEART       TO DN-RAD-KVBEART                      
214300     MOVE TILLK-RADB-BERADREF      TO DN-RAD-BERADREF                     
214400     MOVE TILLK-RADB-IDKUNDRF-RO(3:5) TO DN-RAD-IDKUNDRF-RO               
214500     MOVE TILLK-RADB-KVLEVART      TO DN-RAD-KVLEVART                     
214600     MOVE TILLK-RADB-IDKOLLI       TO DN-RAD-IDKOLLI                      
214700     .                                                                    
214800     EJECT                                                                
214900                                                                          
215000 S26B-FL-VIPSTILLK-RADB-UPPG SECTION.                                     
215100                                                                          
215200     MOVE 'STA-S26 '      TO FELTEXT                                      
215300                                                                          
215400     MOVE VIPSTILLK-RADB-IDARTNR     TO DN-RAD-IDARTNR                    
215500     MOVE '-'                        TO DN-RAD-STRECK                     
215600     MOVE VIPSTILLK-RADB-REKSIFFR    TO DN-RAD-REKSIFFR                   
215700     MOVE VIPSTILLK-RADB-BEART-USA   TO DN-RAD-BEART-USA                  
215800     MOVE VIPSTILLK-RADB-KVBEART     TO DN-RAD-KVBEART                    
215900     MOVE VIPSTILLK-RADB-BERADREF    TO DN-RAD-BERADREF                   
216000     MOVE VIPSTILLK-RADB-IDKUNDRF-RO(3:5) TO                              
216100                               DN-RAD-IDKUNDRF-RO                         
216200     MOVE VIPSTILLK-RADB-KVLEVART    TO DN-RAD-KVLEVART                   
216300     MOVE VIPSTILLK-RADB-IDKOLLI     TO DN-RAD-IDKOLLI                    
216400     .                                                                    
216500     EJECT                                                                
216600                                                                          
216700 S27-PULSOBKR-OCH-LEV-TILLK SECTION.                                      
216800                                                                          
216900     MOVE 'STA-S27 '      TO FELTEXT                                      
217000                                                                          
217100     MOVE LOW-VALUE          TO W-WDQ501KY-TILLK-MIN-X                    
217200     MOVE HIGH-VALUE         TO W-WDQ501KY-TILLK-MAX-X                    
217300     MOVE WS-IDORDER         TO W-RADB-IDORDER-TILLK-MIN                  
217400                                W-RADB-IDORDER-TILLK-MAX                  
217500     MOVE OBKR-IDARTNR-TILLK TO W-RADB-IDARTNR-TILLK-MIN                  
217600                                W-RADB-IDARTNR-TILLK-MAX                  
217700                                                                          
217800     MOVE NEJ TO TRAFF-SW                                                 
217900     PERFORM IMS-GU-RADB-TILLK-MIN-MAX                                    
218000     PERFORM UNTIL ((TILLK-RADB-BASEN-SLUT)                               
218100               OR   (TILLK-RADB-SEGMENT-SAKNAS)                           
218200               OR   (TRAFF))                                              
218300        MOVE TILLK-RADB-IDPURAD  TO WS-SPAR-IDPURAD                       
218400        MOVE TILLK-RADB-IDKOLLI  TO WS-SPAR-IDKOLLI                       
218500        PERFORM UNTIL ((TILLK-RADB-BASEN-SLUT)                            
218600                  OR   (TILLK-RADB-SEGMENT-SAKNAS)                        
218700                  OR   (TRAFF)                                            
218800                  OR   (TILLK-RADB-IDPURAD NOT =                          
218900                                       WS-SPAR-IDPURAD))                  
219000           IF TILLK-RADB-IDDC = OBKR-IDDC                                 
219100            IF TILLK-RADB-FLTILLK = JA                                    
219200              IF TILLK-RADB-KVBEART   = OBKR-KVBEART                      
219300              OR TILLK-RADB-KVBEART-Q = OBKR-KVBEART-Q                    
219400                                                                          
219500               IF TILLK-RADB-IDKOLLI = WS-SPAR-IDKOLLI                    
219600                 MOVE LOW-VALUE  TO W-WDQ101KY-LEV-MIN-X                  
219700                 MOVE HIGH-VALUE TO W-WDQ101KY-LEV-MAX-X                  
219800                 MOVE RADB-IDORDER       TO                               
219900                                    W-OBKR-IDORDER-LEV-MIN                
220000                                    W-OBKR-IDORDER-LEV-MAX                
220100                 MOVE TILLK-RADB-IDARTNR TO                               
220200                                     W-OBKR-IDARTNR-LEV-MIN               
220300                                     W-OBKR-IDARTNR-LEV-MAX               
220400                                                                          
220500                 MOVE TILLK-RADB-KVBEART TO WS-KVBEART                    
220600                 MOVE TILLK-RADB-KVBEART-Q TO WS-KVBEART-Q                
220700                 PERFORM IMS-GU-OBKR-LEV-MIN-MAX                          
220800                 IF SEGMENT-FINNS                                         
220900                    INITIALIZE DN-DETALJRAD                               
221000                    PERFORM UNTIL ((OBKR-LEV-BASEN-SLUT)                  
221100                              OR   (OBKR-LEV-SEGMENT-SAKNAS))             
221200                       IF TILLK-RADB-IDDC = WS-IDDC-LEV                   
221300                        IF LEV-OBKR-FLTILLK = JA                          
221400                          IF WS-KVBEART   = LEV-OBKR-KVBEART              
221500                          OR WS-KVBEART-Q = LEV-OBKR-KVBEART-Q            
221600                           IF TILLK-RADB-IDKUNDRF-RO(3:5) =               
221700                              LEV-OBKR-IDKUNDRF-RO(3:5)                   
221800                             PERFORM S26-FLYTTA-TILLK-RADB-UPPG           
221900                             MOVE PULS TO TILLKOMMANDE-SW                 
222000                             PERFORM S24-PULSORDERBEKR                    
222100                             IF SKRIV-LEVRAD                              
222200                                IF WS-KDORDBEK NOT = SPACE                
222300                                   MOVE WS-KDORDBEK  TO                   
222400                                             RED-KDORDBEK-KOD             
222500                                   MOVE '*' TO RED-KDORDBEK-X             
222600                                   MOVE RED-KDORDBEK TO                   
222700                                             DN-RAD-KDORDBEK              
222800                                ELSE                                      
222900                                   MOVE '*' TO DN-RAD-KDORDBEK            
223000                                END-IF                                    
223100                                MOVE JA TO TRAFF-SW                       
223200                                PERFORM S42-SKRIV-DETALJRAD               
223300                                IF TILLK-RADB-KVLEVART > ZERO             
223400                                   AND                                    
223500                                   TILLK-RADB-IDDC =                      
223600                                               WS-IDDC-LEV                
223700                                   COMPUTE                                
223800                                      WS-SUM-DELIV-LINES =                
223900                                      WS-SUM-DELIV-LINES + 1              
224000                                END-IF                                    
224100                                IF SPARRTEXT                              
224200                                   PERFORM S29-SPARRTEXT                  
224300                                END-IF                                    
224400                             END-IF                                       
224500                           END-IF                                         
224600                          END-IF                                          
224700                        END-IF                                            
224800                       END-IF                                             
224900                       PERFORM IMS-GN-OBKR-LEV-MIN-MAX                    
225000                    END-PERFORM                                           
225100                    IF RADB-FLDIRLEV = JA                                 
225200                       INITIALIZE DN-DETALJRAD                            
225300                       MOVE 'GNB DELIV.' TO DN-RAD-BEART-USA              
225400                       PERFORM S42-SKRIV-DETALJRAD                        
225500                    END-IF                                                
225600                    IF DN-RAD-BEART-USA = SPACE                           
225700*                      NÄR SEGMENT FINNS MEN ORDERBEKRÄFTELSEN            
225800*                      INTE TILLHÖR RÄTT ORDERRAD                         
225900                       PERFORM S27B-SKRIV-RAD                             
226000                    END-IF                                                
226100                 ELSE                                                     
226200                    PERFORM S27B-SKRIV-RAD                                
226300                 END-IF                                                   
226400               ELSE                                                       
226500                  INITIALIZE DN-DETALJRAD                                 
226600                  PERFORM S22-FLYTTA-KOLLIRAD-TILLK                       
226700                  MOVE '*'        TO DN-RAD-KDORDBEK                      
226800                  PERFORM S42-SKRIV-DETALJRAD                             
226900               END-IF                                                     
227000              END-IF                                                      
227100            END-IF                                                        
227200           END-IF                                                         
227300           PERFORM IMS-GN-RADB-TILLK-MIN-MAX                              
227400        END-PERFORM                                                       
227500     END-PERFORM                                                          
227600     .                                                                    
227700     EJECT                                                                
227800                                                                          
227900 S27B-SKRIV-RAD SECTION.                                                  
228000                                                                          
228100     MOVE 'STA-S27B'      TO FELTEXT                                      
228200                                                                          
228300     INITIALIZE DN-DETALJRAD                                              
228400     PERFORM S26-FLYTTA-TILLK-RADB-UPPG                                   
228500     MOVE '*'                  TO DN-RAD-KDORDBEK                         
228600     PERFORM S42-SKRIV-DETALJRAD                                          
228700     IF TILLK-RADB-KVLEVART > ZERO AND                                    
228800        TILLK-RADB-IDDC = WS-IDDC-LEV                                     
228900        COMPUTE WS-SUM-DELIV-LINES =                                      
229000                WS-SUM-DELIV-LINES + 1                                    
229100     END-IF                                                               
229200     IF RADB-FLDIRLEV = JA                                                
229300        INITIALIZE DN-DETALJRAD                                           
229400        MOVE 'GNB DELIV.'      TO DN-RAD-BEART-USA                        
229500        PERFORM S42-SKRIV-DETALJRAD                                       
229600     END-IF                                                               
229700     .                                                                    
229800     EJECT                                                                
229900                                                                          
230000 S28-VIPSTILLK-PULSERS-RAD-1-2 SECTION.                                   
230100                                                                          
230200     MOVE 'STA-S28'      TO FELTEXT                                       
230300                                                                          
230400     INITIALIZE DN-DETALJRAD                                              
230500                                                                          
230600     MOVE VIPSTILLK-RADB-IDARTNR   TO DN-RAD-IDARTNR                      
230700     MOVE '-'                      TO DN-RAD-STRECK                       
230800     MOVE VIPSTILLK-RADB-IDARTNR   TO KSIF-FLT                            
230900     PERFORM S52-BERAKNA-REKSIFFR                                         
231000     MOVE KSIF-KSIFF               TO DN-RAD-REKSIFFR                     
231100     MOVE OBKR-KVBEART             TO DN-RAD-KVBEART                      
231200     MOVE OBKR-KDORDBEK            TO RED-KDORDBEK-KOD                    
231300     MOVE '*'                      TO RED-KDORDBEK-X                      
231400     MOVE RED-KDORDBEK             TO DN-RAD-KDORDBEK                     
231500     MOVE VIPSTILLK-RADB-BEART-USA TO DN-RAD-BEART-USA                    
231600     PERFORM S42-SKRIV-DETALJRAD                                          
231700     IF OBKR-KDORDBEK = 41                                                
231800        INITIALIZE DN-DETALJRAD                                           
231900        MOVE 'REPLACED BY'         TO DN-RAD-BEART-USA                    
232000        MOVE '*'                   TO DN-RAD-KDORDBEK                     
232100        PERFORM S42-SKRIV-DETALJRAD                                       
232200     END-IF                                                               
232300     .                                                                    
232400     EJECT                                                                
232500                                                                          
232600 S29-SPARRTEXT SECTION.                                                   
232700                                                                          
232800     MOVE 'STA-S29'      TO FELTEXT                                       
232900                                                                          
233000     MOVE RADB-IDARTNR          TO W-ARTS-IDARTNR                         
233100     MOVE RADB-IDDC             TO W-ARTS-IDDC                            
233200     PERFORM IMS-GU-ARTS11                                                
233300     IF SEGMENT-FINNS                                                     
233400        IF SLAG-KDLEVSP      > ZERO                                       
233500        OR SLAG-KVSPARR-KVAL > ZERO                                       
233600           INITIALIZE DN-DETALJRAD                                        
233700           MOVE 'CALL PARTS '   TO DN-RAD-BEART-USA                       
233800           PERFORM S42-SKRIV-DETALJRAD                                    
233900                                                                          
234000           INITIALIZE DN-DETALJRAD                                        
234100           MOVE 'SUPPORT    '   TO DN-RAD-BEART-USA                       
234200           PERFORM S42-SKRIV-DETALJRAD                                    
234300        ELSE                                                              
234400           IF SLAG-FLORDSP = JA                                           
234500              IF RADB-IDARTNR = 9452456                                   
234600                 CONTINUE                                                 
234700              ELSE                                                        
234800                 INITIALIZE DN-DETALJRAD                                  
234900                 MOVE 'CALL PARTS ' TO DN-RAD-BEART-USA                   
235000                 PERFORM S42-SKRIV-DETALJRAD                              
235100                                                                          
235200                 INITIALIZE DN-DETALJRAD                                  
235300                 MOVE 'SUPPORT    ' TO DN-RAD-BEART-USA                   
235400                 PERFORM S42-SKRIV-DETALJRAD                              
235500              END-IF                                                      
235600           ELSE                                                           
235700              IF HUV-RADB-KDORDKL > 1                                     
235800                 IF SLAG-FLSPBULK = JA                                    
235900                    INITIALIZE DN-DETALJRAD                               
236000                    MOVE 'CRITICAL   '    TO DN-RAD-BEART-USA             
236100                    PERFORM S42-SKRIV-DETALJRAD                           
236200                                                                          
236300                    INITIALIZE DN-DETALJRAD                               
236400                    MOVE 'ORDER ONLY '    TO DN-RAD-BEART-USA             
236500                    PERFORM S42-SKRIV-DETALJRAD                           
236600                 END-IF                                                   
236700              END-IF                                                      
236800           END-IF                                                         
236900        END-IF                                                            
237000     END-IF                                                               
237100     MOVE NEJ TO SPARRTEXT-SW                                             
237200     .                                                                    
237300     EJECT                                                                
237400                                                                          
237500 S41-SKRIV-RUBRIKER SECTION.                                              
237600                                                                          
237700     MOVE 'STA-S41 '      TO FELTEXT                                      
237800                                                                          
237900     ADD 1                         TO WS-SIDRAK                           
238000     MOVE WS-SIDRAK                TO DN-RUB1-SIDNR                       
238100                                                                          
238200     MOVE MID-IDDC                 TO WS-IDDC                             
238300     IF GMT-FLDNDAP = NEJ AND NDC-NA                                      
238400        CONTINUE                                                          
238500     ELSE                                                                 
238600        IF GMT-FLDNDAP = JA                                               
238700          AND (HUV-RADB-KDORDKL = 0 OR 1 OR 2 OR 3 OR 4)                  
238800                                                                          
238900          PERFORM S90-PUT-DAP-START                                       
239000          MOVE DN-RUBRIK-1            TO SEND-RAD                         
239100          MOVE WS-PAGESKIP            TO STYRTECKEN-RAD                   
239200          PERFORM S90-PUT-DOC-LINE                                        
239300                                                                          
239400          MOVE DN-RUBRIK-2            TO SEND-RAD                         
239500          MOVE WS-SKIP3               TO STYRTECKEN-RAD                   
239600          PERFORM S90-PUT-DOC-LINE                                        
239700                                                                          
239800          MOVE DN-RUBRIK-3            TO SEND-RAD                         
239900          MOVE WS-SKIP2               TO STYRTECKEN-RAD                   
240000          PERFORM S90-PUT-DOC-LINE                                        
240100                                                                          
240200          MOVE DN-RUBRIK-4            TO SEND-RAD                         
240300          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
240400          PERFORM S90-PUT-DOC-LINE                                        
240500                                                                          
240600          MOVE DN-RUBRIK-5            TO SEND-RAD                         
240700          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
240800          PERFORM S90-PUT-DOC-LINE                                        
240900                                                                          
241000          MOVE DN-RUBRIK-6            TO SEND-RAD                         
241100          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
241200          PERFORM S90-PUT-DOC-LINE                                        
241300                                                                          
241400          MOVE DN-RUBRIK-7            TO SEND-RAD                         
241500          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
241600          PERFORM S90-PUT-DOC-LINE                                        
241700                                                                          
241800          MOVE DN-RUBRIK-8            TO SEND-RAD                         
241900          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
242000          PERFORM S90-PUT-DOC-LINE                                        
242100                                                                          
242200          MOVE DN-RUBRIK-9A           TO SEND-RAD                         
242300          MOVE WS-SKIP2               TO STYRTECKEN-RAD                   
242400          PERFORM S90-PUT-DOC-LINE                                        
242500                                                                          
242600          MOVE DN-RUBRIK-9B           TO SEND-RAD                         
242700          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
242800          PERFORM S90-PUT-DOC-LINE                                        
242900                                                                          
243000          MOVE DN-RUBRIK-10           TO SEND-RAD                         
243100          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
243200          PERFORM S90-PUT-DOC-LINE                                        
243300                                                                          
243400          MOVE BLANKRAD               TO SEND-RAD                         
243500          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
243600          PERFORM S90-PUT-DOC-LINE                                        
243700        ELSE                                                              
243800          CALL W006PRS1 USING PRT-SPOOL-A4S                               
243900                              PRT-WRITE                                   
244000                              WS-VALD-PRINTER                             
244100                              ALT-PCB                                     
244200                              PRT-NYSIDA-RAD3                             
244300                              DN-RUBRIK-1                                 
244400                                                                          
244500          CALL W006PRS1 USING PRT-SPOOL-A4S                               
244600                              PRT-WRITE                                   
244700                              WS-VALD-PRINTER                             
244800                              ALT-PCB                                     
244900                              PRT-AFTER-3                                 
245000                              DN-RUBRIK-2                                 
245100                                                                          
245200          CALL W006PRS1 USING PRT-SPOOL-A4S                               
245300                              PRT-WRITE                                   
245400                              WS-VALD-PRINTER                             
245500                              ALT-PCB                                     
245600                              PRT-AFTER-2                                 
245700                              DN-RUBRIK-3                                 
245800                                                                          
245900          CALL W006PRS1 USING PRT-SPOOL-A4S                               
246000                              PRT-WRITE                                   
246100                              WS-VALD-PRINTER                             
246200                              ALT-PCB                                     
246300                              PRT-AFTER-1                                 
246400                              DN-RUBRIK-4                                 
246500                                                                          
246600          CALL W006PRS1 USING PRT-SPOOL-A4S                               
246700                              PRT-WRITE                                   
246800                              WS-VALD-PRINTER                             
246900                              ALT-PCB                                     
247000                              PRT-AFTER-1                                 
247100                              DN-RUBRIK-5                                 
247200                                                                          
247300          CALL W006PRS1 USING PRT-SPOOL-A4S                               
247400                              PRT-WRITE                                   
247500                              WS-VALD-PRINTER                             
247600                              ALT-PCB                                     
247700                              PRT-AFTER-1                                 
247800                              DN-RUBRIK-6                                 
247900                                                                          
248000          CALL W006PRS1 USING PRT-SPOOL-A4S                               
248100                              PRT-WRITE                                   
248200                              WS-VALD-PRINTER                             
248300                              ALT-PCB                                     
248400                              PRT-AFTER-1                                 
248500                              DN-RUBRIK-7                                 
248600                                                                          
248700          CALL W006PRS1 USING PRT-SPOOL-A4S                               
248800                              PRT-WRITE                                   
248900                              WS-VALD-PRINTER                             
249000                              ALT-PCB                                     
249100                              PRT-AFTER-1                                 
249200                              DN-RUBRIK-8                                 
249300                                                                          
249400          CALL W006PRS1 USING PRT-SPOOL-A4S                               
249500                              PRT-WRITE                                   
249600                              WS-VALD-PRINTER                             
249700                              ALT-PCB                                     
249800                              PRT-AFTER-2                                 
249900                              DN-RUBRIK-9A                                
250000                                                                          
250100          CALL W006PRS1 USING PRT-SPOOL-A4S                               
250200                              PRT-WRITE                                   
250300                              WS-VALD-PRINTER                             
250400                              ALT-PCB                                     
250500                              PRT-AFTER-1                                 
250600                              DN-RUBRIK-9B                                
250700                                                                          
250800          CALL W006PRS1 USING PRT-SPOOL-A4S                               
250900                              PRT-WRITE                                   
251000                              WS-VALD-PRINTER                             
251100                              ALT-PCB                                     
251200                              PRT-AFTER-1                                 
251300                              DN-RUBRIK-10                                
251400                                                                          
251500          CALL W006PRS1 USING PRT-SPOOL-A4S                               
251600                              PRT-WRITE                                   
251700                              WS-VALD-PRINTER                             
251800                              ALT-PCB                                     
251900                              PRT-AFTER-1                                 
252000                              BLANKRAD                                    
252100        END-IF                                                            
252200        MOVE +17                      TO WS-RADRAK                        
252300     END-IF                                                               
252400     .                                                                    
252500     EJECT                                                                
252600                                                                          
252700 S42-SKRIV-DETALJRAD SECTION.                                             
252800                                                                          
252900     MOVE 'STA-S42 '      TO FELTEXT                                      
253000                                                                          
253100     IF DN-DETALJRAD = WS-DN-DETALJRAD-LAST                               
253200       COMPUTE WS-SUM-DELIV-LINES =                                       
253300               WS-SUM-DELIV-LINES - 1                                     
253400     ELSE                                                                 
253500       MOVE MID-IDDC        TO WS-IDDC                                    
253600       IF GMT-FLDNDAP = NEJ AND NDC-NA                                    
253700          CONTINUE                                                        
253800       ELSE                                                               
253900         IF GMT-FLDNDAP = JA                                              
254000           AND (HUV-RADB-KDORDKL = 0 OR 1 OR 2 OR 3 OR 4)                 
254100                                                                          
254200           MOVE DN-DETALJRAD            TO SEND-RAD                       
254300           MOVE WS-SKIP1                TO STYRTECKEN-RAD                 
254400           PERFORM S90-PUT-DOC-LINE                                       
254500         ELSE                                                             
254600           CALL W006PRS1 USING PRT-SPOOL-A4S                              
254700                                PRT-WRITE                                 
254800                                WS-VALD-PRINTER                           
254900                                ALT-PCB                                   
255000                                PRT-AFTER-1                               
255100                                DN-DETALJRAD                              
255200         END-IF                                                           
255300         ADD +1                         TO  WS-RADRAK                     
255400       END-IF                                                             
255500     END-IF                                                               
255600     INITIALIZE WS-DN-DETALJRAD-LAST                                      
255700     MOVE DN-DETALJRAD    TO WS-DN-DETALJRAD-LAST                         
255800     .                                                                    
255900     EJECT                                                                
256000                                                                          
256100 S43-SKRIV-SLUTRADER SECTION.                                             
256200                                                                          
256300     MOVE 'STA-S43 '      TO FELTEXT                                      
256400                                                                          
256500     MOVE MID-IDDC        TO WS-IDDC                                      
256600     IF GMT-FLDNDAP = NEJ AND NDC-NA                                      
256700        CONTINUE                                                          
256800     ELSE                                                                 
256900        IF GMT-FLDNDAP = JA                                               
257000          AND (HUV-RADB-KDORDKL = 0 OR 1 OR 2 OR 3 OR 4)                  
257100                                                                          
257200          MOVE DN-SLUTRAD-1              TO SEND-RAD                      
257300          MOVE WS-SKIP3                  TO STYRTECKEN-RAD                
257400          PERFORM S90-PUT-DOC-LINE                                        
257500          ADD +1                         TO WS-RADRAK                     
257600                                                                          
257700          MOVE DN-SLUTRAD-2              TO SEND-RAD                      
257800          MOVE WS-SKIP1                  TO STYRTECKEN-RAD                
257900          PERFORM S90-PUT-DOC-LINE                                        
258000          ADD +1                         TO  WS-RADRAK                    
258100                                                                          
258200          MOVE DN-SLUTRAD-3              TO SEND-RAD                      
258300          MOVE WS-SKIP1                  TO STYRTECKEN-RAD                
258400          PERFORM S90-PUT-DOC-LINE                                        
258500          ADD +1                         TO  WS-RADRAK                    
258600                                                                          
258700          MOVE DN-SLUTRAD-4              TO SEND-RAD                      
258800          MOVE WS-SKIP1                  TO STYRTECKEN-RAD                
258900          PERFORM S90-PUT-DOC-LINE                                        
259000          ADD +1                         TO  WS-RADRAK                    
259100                                                                          
259200          MOVE DN-ERROR-LINE             TO SEND-RAD                      
259300          MOVE WS-SKIP1                  TO STYRTECKEN-RAD                
259400          PERFORM S90-PUT-DOC-LINE                                        
259500        ELSE                                                              
259600          CALL W006PRS1 USING PRT-SPOOL-A4S                               
259700                              PRT-WRITE                                   
259800                              WS-VALD-PRINTER                             
259900                              ALT-PCB                                     
260000                              PRT-AFTER-3                                 
260100                              DN-SLUTRAD-1                                
260200          ADD +1                      TO  WS-RADRAK                       
260300                                                                          
260400          CALL W006PRS1 USING PRT-SPOOL-A4S                               
260500                              PRT-WRITE                                   
260600                              WS-VALD-PRINTER                             
260700                              ALT-PCB                                     
260800                              PRT-AFTER-1                                 
260900                              DN-SLUTRAD-2                                
261000          ADD +1                      TO  WS-RADRAK                       
261100                                                                          
261200          CALL W006PRS1 USING PRT-SPOOL-A4S                               
261300                              PRT-WRITE                                   
261400                              WS-VALD-PRINTER                             
261500                              ALT-PCB                                     
261600                              PRT-AFTER-1                                 
261700                              DN-SLUTRAD-3                                
261800          ADD +1                      TO  WS-RADRAK                       
261900                                                                          
262000          CALL W006PRS1 USING PRT-SPOOL-A4S                               
262100                              PRT-WRITE                                   
262200                              WS-VALD-PRINTER                             
262300                              ALT-PCB                                     
262400                              PRT-AFTER-1                                 
262500                              DN-SLUTRAD-4                                
262600          ADD +1                      TO  WS-RADRAK                       
262700                                                                          
262800          CALL W006PRS1 USING PRT-SPOOL-A4S                               
262900                              PRT-WRITE                                   
263000                              WS-VALD-PRINTER                             
263100                              ALT-PCB                                     
263200                              PRT-AFTER-1                                 
263300                              DN-ERROR-LINE                               
263400        END-IF                                                            
263500        ADD +1                           TO  WS-RADRAK                    
263600     END-IF                                                               
263700     .                                                                    
263800     EJECT                                                                
263900                                                                          
264000 S51-HAMTA-BENAMNING SECTION.                                             
264100                                                                          
264200     MOVE 'STA-S51 '               TO FELTEXT                             
264300                                                                          
264400     MOVE SPACE                    TO SSA1 SSA2                           
264500     PERFORM IMS-GET-BENA11-BSEQ                                          
264600     IF SEGMENT-FINNS                                                     
264700        MOVE TEXT-BEART            TO WS-BEART-USA                        
264800     ELSE                                                                 
264900        MOVE 'UNKNOWN    '         TO WS-BEART-USA                        
265000     END-IF                                                               
265100     .                                                                    
265200     EJECT                                                                
265300                                                                          
265400 S52-BERAKNA-REKSIFFR SECTION.                                            
265500                                                                          
265600     MOVE 'STA-S52 '                TO FELTEXT                            
265700                                                                          
265800     MOVE +9                        TO KSIF-LGD                           
265900     CALL W009KSIF USING KSIF-FLT KSIF-LGD KSIF-KSIFF                     
266000     .                                                                    
266100     EJECT                                                                
266200                                                                          
266300 S90-SEND-OPEN SECTION.                                                   
266400     MOVE 'OPEN'                        TO SEND-KDFUNC                    
266500     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
266600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
266700                         SEND-OPEN-AREA                                   
266800     IF SEND-KDRC > 0                                                     
266900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
267000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
267100       DELIMITED BY SIZE INTO FELTEXT                                     
267200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
267300     END-IF                                                               
267400     .                                                                    
267500     EJECT                                                                
267600 S90-PUT-DAP-START SECTION.                                               
267700                                                                          
267800     MOVE 1                       TO REQU-IDMSGVER                        
267900     MOVE 'R'                     TO REQU-KDPGMACT                        
268000     MOVE IDPGM                   TO REQU-IDUSER                          
268100     MOVE 'DELNOTE'               TO HDR-IDOUTTYPE                        
268200     MOVE SPACE                   TO HDR-IDOUTREC                         
268300                                     HDR-IDLIST                           
268400     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
268500     MOVE MID-IDDC                TO WS-IDDC                              
268600     IF NDC-US AND DIST07-CAN-PRINT-DNOTE                                 
268700       MOVE 'CA'                  TO HDR-IDOUTREC (1:2)                   
268800     ELSE                                                                 
268900       MOVE DCS-IDLANDX2          TO HDR-IDOUTREC (1:2)                   
269000     END-IF                                                               
269100     MOVE WS-IDKUNDNR             TO WS-IDKUNDNR-DAP                      
269200     MOVE WS-IDKUNDNR-DAP         TO HDR-IDOUTREC (3:7)                   
269300     MOVE WS-IDPRODNR             TO HDR-IDLIST                           
269400     MOVE 'PUT'                   TO SEND-KDFUNC                          
269500     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
269600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
269700                         SEND-KVDLEN                                      
269800                         HDR-AREA                                         
269900     IF SEND-KDRC > ZERO                                                  
270000       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
270100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
270200       DELIMITED BY SIZE INTO FELTEXT-STR                                 
270300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
270400     END-IF                                                               
270500     .                                                                    
270600     EJECT                                                                
270700 S90-PUT-DOC-LINE SECTION.                                                
270800     MOVE 'PUT'                           TO SEND-KDFUNC                  
270900     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
271000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
271100                         SEND-KVDLEN                                      
271200                         SEND-RAD                                         
271300     IF SEND-KDRC > ZERO                                                  
271400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
271500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
271600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
271700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
271800     END-IF                                                               
271900     .                                                                    
272000     EJECT                                                                
272100 S90-SEND-CLOSE SECTION.                                                  
272200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
272300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
272400                                                                          
272500     IF SEND-KDRC > 0                                                     
272600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
272700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
272800       DELIMITED BY SIZE INTO FELTEXT                                     
272900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
273000     END-IF                                                               
273100     .                                                                    
273200     EJECT                                                                
273300* --- IMS SEKTIONER ---                                                   
273400                                                                          
273500 IMS-GET-MSG SECTION.                                                     
273600                                                                          
273700     MOVE '  QC' TO GODK-STATUSKODER                                      
273800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
273900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
274000     PERFORM IMS-STATUSKONTROLL                                           
274100     .                                                                    
274200     EJECT                                                                
274300                                                                          
274400 IMS-GU-WDE6-VORD SECTION.                                                
274500                                                                          
274600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
274700          DELIMITED BY SIZE INTO SSA1                                     
274800     MOVE '  GE' TO GODK-STATUSKODER                                      
274900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
275000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
275100     PERFORM IMS-STATUSKONTROLL                                           
275200     .                                                                    
275300     EJECT                                                                
275400                                                                          
275500 IMS-GNP-WDE6-KOLLI SECTION.                                              
275600                                                                          
275700     MOVE 'WDE611   ' TO SSA1                                             
275800     MOVE '  GE' TO GODK-STATUSKODER                                      
275900     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
276000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
276100     PERFORM IMS-STATUSKONTROLL                                           
276200     .                                                                    
276300     EJECT                                                                
276400                                                                          
276500 IMS-GU-RADB-HUV-MIN-MAX SECTION.                                         
276600                                                                          
276700     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-HUV-MIN-X                    
276800                    '&WDQ501KY<=' W-WDQ501KY-HUV-MAX-X ')'                
276900          DELIMITED BY SIZE INTO SSA1                                     
277000     MOVE '  GE' TO GODK-STATUSKODER                                      
277100     CALL CBLTDLI USING GU ORQP2-PCB DLI-IO-ORQP01-HUV SSA1               
277200     MOVE ORQP2-STATUS-CODE TO STATUS-WS                                  
277300     PERFORM IMS-STATUSKONTROLL                                           
277400     .                                                                    
277500     EJECT                                                                
277600                                                                          
277700 IMS-GU-RADB-MIN-MAX SECTION.                                             
277800                                                                          
277900     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
278000                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
278100          DELIMITED BY SIZE INTO SSA1                                     
278200     MOVE '  GE' TO GODK-STATUSKODER                                      
278300     CALL CBLTDLI USING GU ORQP-PCB DLI-IO-ORQP01 SSA1                    
278400     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
278500                              RADB-STATUS-WS                              
278600     PERFORM IMS-STATUSKONTROLL                                           
278700     .                                                                    
278800     EJECT                                                                
278900                                                                          
279000 IMS-GN-RADB-MIN-MAX SECTION.                                             
279100                                                                          
279200     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-MIN-X                        
279300                    '&WDQ501KY<=' W-WDQ501KY-MAX-X ')'                    
279400          DELIMITED BY SIZE INTO SSA1                                     
279500     MOVE '  GE' TO GODK-STATUSKODER                                      
279600     CALL CBLTDLI USING GN ORQP-PCB DLI-IO-ORQP01 SSA1                    
279700     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
279800                              RADB-STATUS-WS                              
279900     PERFORM IMS-STATUSKONTROLL                                           
280000     .                                                                    
280100     EJECT                                                                
280200                                                                          
280300 IMS-GU-RADB-TILLK-MIN-MAX SECTION.                                       
280400                                                                          
280500     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-TILLK-MIN-X                  
280600                    '&WDQ501KY<=' W-WDQ501KY-TILLK-MAX-X ')'              
280700          DELIMITED BY SIZE INTO SSA1                                     
280800     MOVE '  GE' TO GODK-STATUSKODER                                      
280900     CALL CBLTDLI USING GU ORQP3-PCB DLI-IO-ORQP01-TILLK SSA1             
281000     MOVE ORQP3-STATUS-CODE TO STATUS-WS TILLK-RADB-STATUS-WS             
281100     PERFORM IMS-STATUSKONTROLL                                           
281200     .                                                                    
281300     EJECT                                                                
281400                                                                          
281500 IMS-GN-RADB-TILLK-MIN-MAX SECTION.                                       
281600                                                                          
281700     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-TILLK-MIN-X                  
281800                    '&WDQ501KY<=' W-WDQ501KY-TILLK-MAX-X ')'              
281900          DELIMITED BY SIZE INTO SSA1                                     
282000     MOVE '  GE' TO GODK-STATUSKODER                                      
282100     CALL CBLTDLI USING GN ORQP3-PCB DLI-IO-ORQP01-TILLK SSA1             
282200     MOVE ORQP3-STATUS-CODE TO STATUS-WS TILLK-RADB-STATUS-WS             
282300     PERFORM IMS-STATUSKONTROLL                                           
282400     .                                                                    
282500     EJECT                                                                
282600                                                                          
282700 IMS-GU-RADB-VIPSTILLK-MIN-MAX SECTION.                                   
282800                                                                          
282900     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-VIPSTILLK-MIN-X              
283000                    '&WDQ501KY<=' W-WDQ501KY-VIPSTILLK-MAX-X ')'          
283100          DELIMITED BY SIZE INTO SSA1                                     
283200     MOVE '  GE' TO GODK-STATUSKODER                                      
283300     CALL CBLTDLI USING GU ORQP2-PCB DLI-IO-ORQP01-VIPSTILLK SSA1         
283400     MOVE ORQP2-STATUS-CODE TO STATUS-WS VIPSTILLK-RADB-STATUS-WS         
283500     PERFORM IMS-STATUSKONTROLL                                           
283600     .                                                                    
283700     EJECT                                                                
283800                                                                          
283900 IMS-GN-RADB-VIPSTILLK-MIN-MAX SECTION.                                   
284000                                                                          
284100     STRING 'WLORQP01(WDQ501KY=>' W-WDQ501KY-VIPSTILLK-MIN-X              
284200                    '&WDQ501KY<=' W-WDQ501KY-VIPSTILLK-MAX-X ')'          
284300          DELIMITED BY SIZE INTO SSA1                                     
284400     MOVE '  GE' TO GODK-STATUSKODER                                      
284500     CALL CBLTDLI USING GN ORQP2-PCB DLI-IO-ORQP01-VIPSTILLK SSA1         
284600     MOVE ORQP2-STATUS-CODE TO STATUS-WS VIPSTILLK-RADB-STATUS-WS         
284700     PERFORM IMS-STATUSKONTROLL                                           
284800     .                                                                    
284900     EJECT                                                                
285000                                                                          
285100 IMS-GU-OBKR-MIN-MAX       SECTION.                                       
285200                                                                          
285300     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
285400                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
285500          DELIMITED BY SIZE INTO SSA1                                     
285600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
285700     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-ORQM01 SSA1                    
285800     MOVE ORQM-STATUS-CODE TO STATUS-WS OBKR-STATUS-WS                    
285900     PERFORM IMS-STATUSKONTROLL                                           
286000     .                                                                    
286100     EJECT                                                                
286200                                                                          
286300 IMS-GN-OBKR-MIN-MAX       SECTION.                                       
286400                                                                          
286500     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
286600                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
286700          DELIMITED BY SIZE INTO SSA1                                     
286800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
286900     CALL CBLTDLI USING GN ORQM-PCB DLI-IO-ORQM01 SSA1                    
287000     MOVE ORQM-STATUS-CODE TO STATUS-WS OBKR-STATUS-WS                    
287100     PERFORM IMS-STATUSKONTROLL                                           
287200     .                                                                    
287300     EJECT                                                                
287400                                                                          
287500 IMS-GU-OBKR-LEV-MIN-MAX    SECTION.                                      
287600                                                                          
287700     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-LEV-MIN-X                    
287800                    '&WDQ101KY<=' W-WDQ101KY-LEV-MAX-X ')'                
287900          DELIMITED BY SIZE INTO SSA1                                     
288000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
288100     CALL CBLTDLI USING GU ORQM2-PCB DLI-IO-ORQM01-LEV SSA1               
288200     MOVE ORQM2-STATUS-CODE TO STATUS-WS OBKR-LEV-STATUS-WS               
288300     PERFORM IMS-STATUSKONTROLL                                           
288400     .                                                                    
288500     EJECT                                                                
288600                                                                          
288700 IMS-GN-OBKR-LEV-MIN-MAX    SECTION.                                      
288800                                                                          
288900     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-LEV-MIN-X                    
289000                    '&WDQ101KY<=' W-WDQ101KY-LEV-MAX-X ')'                
289100          DELIMITED BY SIZE INTO SSA1                                     
289200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
289300     CALL CBLTDLI USING GN ORQM2-PCB DLI-IO-ORQM01-LEV SSA1               
289400     MOVE ORQM2-STATUS-CODE TO STATUS-WS OBKR-LEV-STATUS-WS               
289500     PERFORM IMS-STATUSKONTROLL                                           
289600     .                                                                    
289700     EJECT                                                                
289800                                                                          
289900 IMS-GU-4459-BARN SECTION.                                                
290000                                                                          
290100     STRING 'WL445901(WDGXKEY  =' W-WDGX4459-X ')'                        
290200          DELIMITED BY SIZE INTO SSA1                                     
290300     STRING 'WL445911(KY4460   =' W-WDGX4460-X ')'                        
290400          DELIMITED BY SIZE INTO SSA2                                     
290500     MOVE '  GE' TO GODK-STATUSKODER                                      
290600     CALL CBLTDLI USING GU 4459-PCB DLI-IO-4460 SSA1 SSA2                 
290700     MOVE 4459-STATUS-CODE TO STATUS-WS                                   
290800     PERFORM IMS-STATUSKONTROLL                                           
290900     .                                                                    
291000     EJECT                                                                
291100                                                                          
291200 IMS-GU-ARTS11 SECTION.                                                   
291300                                                                          
291400     STRING 'WLARTS01(IDARTNR  =' W-ARTS-IDARTNR-X ')'                    
291500          DELIMITED BY SIZE INTO SSA1                                     
291600     STRING 'WLARTS11(IDDC     =' W-ARTS-IDDC ')'                         
291700          DELIMITED BY SIZE INTO SSA2                                     
291800     MOVE '  GE' TO GODK-STATUSKODER                                      
291900     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
292000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
292100     PERFORM IMS-STATUSKONTROLL                                           
292200     .                                                                    
292300     EJECT                                                                
292400                                                                          
292500 IMS-GET-BENA11-BSEQ  SECTION.                                            
292600     SKIP3                                                                
292700     STRING 'WLBENA01(WDD3BSEQ =' W-BENA-IDARTNR-X ')'                    
292800            DELIMITED BY SIZE INTO SSA1                                   
292900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
293000            DELIMITED BY SIZE INTO SSA2                                   
293100     MOVE '  GE' TO GODK-STATUSKODER                                      
293200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-BENA11 SSA1 SSA2               
293300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
293400     PERFORM IMS-STATUSKONTROLL                                           
293500     .                                                                    
293600     EJECT                                                                
293700 IMS-GU-WDB201 SECTION.                                                   
293800                                                                          
293900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
294000          DELIMITED BY SIZE INTO SSA1                                     
294100     MOVE '  ' TO GODK-STATUSKODER                                        
294200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
294300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
294400     PERFORM IMS-STATUSKONTROLL                                           
294500     .                                                                    
294600     EJECT                                                                
294700 IMS-GU-WDB601    SECTION.                                                
294800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
294900          DELIMITED BY SIZE INTO SSA1                                     
295000     MOVE '  GE' TO GODK-STATUSKODER                                      
295100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
295200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
295300     PERFORM IMS-STATUSKONTROLL                                           
295400     IF SEGMENT-SAKNAS                                                    
295500         MOVE SPACE TO DCS-KDDC                                           
295600                       DCS-IDDC                                           
295700                       DCS-IDLANDX2                                       
295800     END-IF                                                               
295900     .                                                                    
296000                                                                          
296100 IMS-STATUSKONTROLL SECTION.                                              
296200                                                                          
296300     SET STATUS-IX TO 1                                                   
296400     SEARCH GODK-STATUS                                                   
296500       AT END                                                             
296600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
296700         DELIMITED BY SIZE INTO FELTEXT                                   
296800         CALL FELLOG                                                      
296900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
297000         CONTINUE                                                         
297100     END-SEARCH                                                           
297200     .                                                                    
