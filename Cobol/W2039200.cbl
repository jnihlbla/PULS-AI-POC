000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2039200.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   2015/10/06..                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        MANUAL BUY FROM CHINA NDC TO CDC .                               
000900*                                                                         
001000*        ORDER PROPOSAL FROM REFILL SYSTEMS SHOWS ON SCREEN.              
001100*        WE CAN APPROVE, REJECT OR CHANGE THESE.                          
001200*                                                                         
001300*        ENTER :    USED FOR SIMULATION OF SUPERWEEK                      
001400*        PF 11 :    UPDATING                                              
001500*                   1) ORDER PROPOSAL WITH QUANTITY WILL                  
001600*                      CREATE AN ORDER                                    
001700*                   2) ORDER PROPOSAL WITH ZERO IN QUANTITY WILL          
001800*                      DELETE THE PROPOSAL                                
001900*                   3) WE CAN MANUALLY ENTER A QUANTITY TO                
002000*                      CREATE AN ORDER                                    
002100*                   4) WE CAN SET FORECAST, KVPB-SEP, MANUALLY.           
002200*                      THIS WILL RECALULATE THE ORDER POINT AND           
002300*                      REFILL QUANTITY AND KVPB-PLAN  AND UPDATE          
002400*                      WDK6 DATABASE                                      
002500*                   5) WE CAN UPDATE THE FLAG FOR 'ALW AIR',              
002600*                      AUTO REFILL AND COMMENTS                           
002700*                                                                         
002800*        PF 7          SHOWS FIRST PROPOSAL                               
002900*        PF 8          SHOWS NEXT PROPOSAL. THIS CAN ALSO BE              
003000*                      UPDATED AS DESCRIBED ABOVE WITH PF11               
003100*                                                                         
003200*        PROGRAM    UPDATES    WDK7                                       
003300*                              WLUSEA (WDP7)                              
003400*                              WDE3                                       
003500*        PROGRAM    READS      WDD3                                       
003600*                              WDA5A                                      
003600*                              WDA5                                       
003700*                              WDK6                                       
003800*                              WDN6                                       
003900*                              WDD7                                       
004000*                              WDK9                                       
004100*                              WDL8                                       
004200*                              WDL6                                       
004300*                                                                         
004310*                                                                         
004400*    INDATA.                                                              
004500*        TRANSAKTION: W2T392                                              
004600*                     W2T392U                                             
004700*        MID:         W2I39201                                            
004800*                                                                         
004900*    UTDATA.                                                              
005000*        MOD:         W2O39201                                            
005100*                                                                         
005200                                                                          
005300     SKIP3                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700 WORKING-STORAGE SECTION.                                                 
005800*    -COPY WY2000W1                                                       
005900     SKIP3                                                                
006000 77  IDPGM                       PIC X(08)   VALUE 'W2039200'.            
006100                                                                          
006200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006400                                                                          
006500 77  YES                         PIC X       VALUE 'Y'.                   
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  AKTIV                       PIC X       VALUE 'A'.                   
006900 77  PASSIV                      PIC X       VALUE 'P'.                   
007000 77  MOD-IX                      PIC 9(3)    VALUE ZERO.                  
007100 77  IX                          PIC 9(3)    VALUE ZERO.                  
007200 77  IX-VV                       PIC 9(2)    VALUE ZERO.                  
007300 77  IX-VV-1                     PIC 9(2)    VALUE ZERO.                  
007400 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
007500 77  WS-TIPBDAT                  PIC S9(5)   VALUE ZERO COMP-3.           
007600 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
007800*                                                                         
007900*01  -COPY WWDC99                                                         
008000*                                                                         
008100*01  -COPY WWDC99          -PRE   REF-                                    
008200                                                                          
008300*01  -COPY WWDIST35                                                       
008400                                                                          
008500 01  DAGENS-PER                  PIC 9(4)   VALUE ZERO.                   
008600 01  DAG-PER REDEFINES DAGENS-PER.                                        
008700         05 DAGENS-AA            PIC 9(2).                                
008800         05 DAGENS-PP            PIC 9(2).                                
008900 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
009000 77  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
009100       EJECT                                                              
009200 01  WS.                                                                  
009300  05 WS-TEST-1.                                                           
009400     10  WS-A                    PIC X     VALUE SPACE.                   
009500     10  FILLER                  PIC X     VALUE '/'.                     
009600     10  WS-B                    PIC X(7)  VALUE SPACE.                   
009700                                                                          
009800  05 WS-TEST-2.                                                           
009900     10  WS-A2                   PIC X     VALUE SPACE.                   
010000     10  FILLER                  PIC X     VALUE '/'.                     
010100                                                                          
010200  05 WS-TEMFSINF.                                                         
010300    10 WS-TEMFSINF-SOURCE        PIC X(07)   VALUE SPACE.                 
010400    10 FILLER                    PIC X       VALUE SPACE.                 
010500    10 FILLER.                                                            
010600      15 WS-TEMFSINF-NDC         PIC X(10)   VALUE SPACE.                 
010700      15 FILLER                  PIC X       VALUE SPACE.                 
010800    10 WS-TEMFSINF-TEXT          PIC X(13)   VALUE SPACE.                 
010900                                                                          
011000  05 WS-TEMF-RED-OS.                                                      
011100    10 WS-TEMF-TEXT-OS           PIC X(3)    VALUE SPACE.                 
011200    10 WS-TEMF-OS-NDC            PIC X(3)    VALUE SPACE.                 
011300                                                                          
011400  05 WS-TEMF-RED-INVBAL.                                                  
011500    10 FILLER                    PIC X(5)    VALUE 'INVB '.               
011600    10 WS-TEMF-UTRSALDO          PIC -(6)9.                               
011700    10 FILLER                    PIC X       VALUE SPACE.                 
011800                                                                          
011900  05 WS-TEMF-UTRSALDO-NUM        PIC S9(7).                               
012001  05 WS-REAIRCO                  PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012002  05 WS-PRFRAKT                  PIC S9(7) VALUE ZERO COMP-3.             
012003  05 WS-AIR-COST-SEK             PIC 9(7) VALUE ZERO.                     
012101                                                                          
012201  05 WS-ANT-PP                   PIC  9(2)   VALUE ZERO.                  
012301  05 WS-ANT-VV                   PIC  9(2)   VALUE ZERO.                  
012401  05 WS-TIAAVV.                                                           
012501    10 WS-AAR                    PIC  9(2)   VALUE ZERO.                  
012601    10 WS-VV                     PIC  9(2)   VALUE ZERO.                  
012701  05 TIAAVV REDEFINES WS-TIAAVV PIC 9(4).                                 
012801  05 WS-TIAAPER.                                                          
012901    10 TIAA                      PIC  9(2)   VALUE ZERO.                  
013001    10 PER                       PIC  9(2)   VALUE ZERO.                  
013101  05    TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
013201  05 WS-TIAAPP                   PIC  9(4)   VALUE ZERO.                  
013301  05 FILLER REDEFINES WS-TIAAPP.                                          
013401    10 WS-TIAAPP-AA              PIC  9(2).                               
013501    10 WS-TIAAPP-PP              PIC  9(2).                               
013601  05 WS-FOM-TOM.                                                          
013701    10 WS-FOM                    PIC  X(2)   VALUE ZERO.                  
013801    10 WS-STRECK                 PIC  X(1)   VALUE '-'.                   
013901    10 WS-TOM                    PIC  X(2)   VALUE ZERO.                  
014001  05 WS-INDATE.                                                           
014101    10 FILLER                    PIC  9(2)   VALUE ZERO.                  
014201    10 WS-INDATE-6               PIC  9(6)   VALUE ZERO.                  
014301                                                                          
014401*                                                                         
014501*   BELOW TABLES USED FOR PERIOD CALCULATIONS                             
014601*                                                                         
014701  05 WS-PERIODTABELL-AR-1        OCCURS 12.                               
014801    10 WS-PER-TIAAPP-1           PIC  9(2)   VALUE ZERO.                  
014901    10 WS-PER-STA-VV-1           PIC  9(2)   VALUE ZERO.                  
015001    10 WS-PER-END-VV-1           PIC  9(2)   VALUE ZERO.                  
015101    10 WS-KVOI-VV-1              PIC S9(7)   VALUE ZERO.                  
015201  05 WS-PERIODTABELL-AR-0        OCCURS 12.                               
015301    10 WS-PER-TIAAPP-0           PIC  9(2)   VALUE ZERO.                  
015401    10 WS-PER-STA-VV-0           PIC  9(2)   VALUE ZERO.                  
015501    10 WS-PER-END-VV-0           PIC  9(2)   VALUE ZERO.                  
015601    10 WS-KVOI-VV-0              PIC S9(7)   VALUE ZERO.                  
015701  05 WS-PERIODTABELL-SORTED      OCCURS 12.                               
015801    10 WS-PER-TIAAPP-S           PIC  9(2)   VALUE ZERO.                  
015901    10 WS-PER-STA-VV-S           PIC  9(2)   VALUE ZERO.                  
016001    10 WS-PER-END-VV-S           PIC  9(2)   VALUE ZERO.                  
016101    10 WS-KVOI-VV-S              PIC S9(7)   VALUE ZERO.                  
016201  05 WS-BALANCE                  PIC S9(7)   VALUE ZERO.                  
016301  05 WS-REST                     PIC S9(7)   VALUE ZERO.                  
016401  05 WS-SLASK                    PIC S9(7)   VALUE ZERO.                  
016501  05 WS-KDERS                    PIC 9(3)    VALUE ZERO.                  
016601  05 WS-FOREG-AAR                PIC  9(4)   VALUE ZERO.                  
016701  05 WS-KVROS-SDC                PIC S9(7)   VALUE ZERO.                  
016801  05 WS-ANTAL-POSTER             PIC S9(7)   VALUE ZERO.                  
016901  05 WS-ANTAL-VECKOR             PIC S9(3)   VALUE ZERO.                  
017001  05 WS-KVOI-SUM                 PIC S9(7)   VALUE ZERO.                  
017101  05 WS-KVAVIS                   PIC S9(9)   VALUE ZERO.                  
017201  05 WS-PREADV-QTY               PIC S9(9)   VALUE ZERO.                  
017301  05 WS-ANTAL                    PIC 9(9)    VALUE ZERO.                  
017401  05 WS-DAPRLIST                 PIC 9(8)    VALUE ZERO.                  
017501  05 WS-DAPUBL                   PIC 9(5)    VALUE ZERO.                  
017601  05 WS-EOP                      PIC 9(5)    VALUE ZERO.                  
017701  05 WS-DAPBPLAN-K6              PIC 9(8)    VALUE ZERO.                  
017801  05 WS-SPARA-IDARTNR            PIC 9(9)    VALUE ZERO.                  
017901  05 W-IDARTNR-SAVED             PIC 9(9)    VALUE ZERO.                  
018001  05 WS-SPARA-IDDC               PIC 9(2)    VALUE ZERO.                  
018002  05 W-IDDC-REF-SAVED            PIC 9(2)    VALUE ZERO.                  
018101  05 WS-FAKTOR-SEK               PIC 9(3)    VALUE ZERO.                  
018201  05 WS-KR-VIKT                  PIC S9(9)V9(2)                           
018301                                             VALUE ZERO.                  
018401  05 WS-KR-VIKT-RED              PIC  9(9)   VALUE ZERO.                  
018501  05 WS-KR-VOLYM                 PIC S9(9)V9(2)                           
018601                                             VALUE ZERO.                  
018701  05 WS-KR-VOLYM-RED             PIC  9(9)   VALUE ZERO.                  
018801  05 WS-KVAKS                    PIC S9(9)V9(2)                           
018901                                             VALUE ZERO.                  
019001  05 WS-KVROS-CDC                PIC S9(6)   VALUE ZERO.                  
019101  05 WS-KVPB                     PIC S9(9)V9(2)                           
019201                                             VALUE ZERO.                  
019301  05 WS-KVPB-SDC                 PIC S9(9)V9(2)                           
019401                                             VALUE ZERO.                  
019501  05 WS-SUPERWEEK                PIC S9(9)V9(2)                           
019601                                             VALUE ZERO.                  
019701  05 WS-AVAILABLE                PIC S9(7)   VALUE ZERO.                  
019801  05 WS-ORDERED                  PIC S9(7)   VALUE ZERO.                  
019901  05 WS-KVAKS-SDC                PIC  9(7)   VALUE ZERO.                  
020001  05 WS-PURCHQTY                 PIC  9(7)   VALUE ZERO.                  
020101  05 WS-PURCHQTY-SIM             PIC  9(7)   VALUE ZERO.                  
020201  05 WS-RED-PURCHQTY             PIC Z(6)9   VALUE ZERO.                  
020301  05 WS-KVPB-SEP                 PIC  9(6)V9 VALUE ZERO.                  
020401  05 WS-KVPB-SEP-S               PIC  9(6)V9 VALUE ZERO.                  
020501  05 WS-KVPB-PLAN                PIC  9(6)V9 VALUE ZERO.                  
020601  05 WS-RED-KVPB-SEP             PIC Z(5)9.9 VALUE ZERO.                  
020701  05 WS-ANT-REVIEW               PIC S9(5)   VALUE ZERO.                  
020801  05 WS-KTRL-PRIO                PIC 9(2)    VALUE ZERO.                  
020901  05 WS-RESTKVANT                PIC S9(7)   VALUE ZERO.                  
021001  05 WS-IDREFTYP                 PIC X       VALUE SPACE.                 
021002  05 WS-where                    PIC X       VALUE SPACE.                 
021101  05 WS-KVOKS-TOT                PIC S9(7)   VALUE ZERO COMP-3.           
021201  05 WS-IDDISTR                  PIC 9(4).                                
021301  05 WS-REF-KDREFTXT             PIC 9(2)    VALUE ZERO.                  
021501  05 WS-FIRST-ETA                PIC 9(6)    VALUE 999999.                
021601  05 WS-FIRST-ETA-QTY            PIC 9(7)    VALUE ZERO.                  
021701  05 WS-SECOND-ETA               PIC 9(6)    VALUE 999999.                
021801  05 WS-SECOND-ETA-QTY           PIC 9(7)    VALUE ZERO.                  
021901  05 WS-KDFRAKT                  PIC S9(3)   VALUE ZERO COMP-3.           
022001  05 WS-FLAGGA-FCD               PIC X       VALUE SPACE.                 
022101  05 WS-FLAGGA-FCD-S             PIC X       VALUE SPACE.                 
022201  05 WS-FLREFBEO-S               PIC X       VALUE SPACE.                 
022301  05 WS-COMMENT-1                PIC X(36)   VALUE SPACE.                 
022401  05 WS-COMMENT-2                PIC X(36)   VALUE SPACE.                 
022501  05 WS-VKART                    PIC S9(7)   VALUE ZERO COMP-3.           
022601  05 WS-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
022701  05 WS-KVQPACK-3                PIC S9(5)  VALUE ZERO COMP-3.            
022801  05 WS-KVVORKO                  PIC S9(7)  VALUE ZERO COMP-3.            
022901                                                                          
023001  05 WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
023101  05 WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
023201  05 FL-PRARTBES                 PIC X       VALUE 'N'.                   
023301  05 WS-PRARTBES                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
023401  05 WS-RESEASON-PLAN  OCCURS 12 PIC S9V9(2) COMP-3.                      
023501                                                                          
023601*********************************************************                 
023701*    WS-MSGI-AREA-2392                                                    
023801*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
023901*           (I MSGI-SPAR-AREA)                                            
024001*********************************************************                 
024101  05 WS-MSGI-AREA-2392.                                                   
024201    10 WS-MSGI-IDTRANS-2392      PIC X(4)    VALUE '2392'.                
024301    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
024401    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
024501    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
024601    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
024701    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
024702    10 WS-MSGI-IDDC-REF          PIC X(2)    VALUE SPACE.                 
024801                                                                          
024901*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
025001                                                                          
025101  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
025201  05 WS-IDARTNR-NUM              REDEFINES WS-IDARTNR                     
025301                                 PIC 9(9).                                
025401  05 IDDC-WS                     PIC X(2)    VALUE SPACE.                 
025501  05 IDDC-WS-NUM                 REDEFINES IDDC-WS                        
025601                                 PIC 9(2).                                
025701  05 WS-IDPERSON-BUY             PIC X(3)    VALUE SPACE.                 
025801  05 WS-IDPERSON-BUY-NUM         REDEFINES WS-IDPERSON-BUY                
025901                                 PIC 9(3).                                
026001  05 WS-IDPERSON-BUY-RED         PIC Z(2)9.                               
026101  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
026102  05 WS-IDDC-REF-KEY             PIC X(2)    VALUE SPACE.                 
026201  05 WS-STATUS                   PIC X       VALUE SPACE.                 
026301                                                                          
026401 01   WS-TESTFAELT.                                                       
026501   03 WS-IDARTNR-TF              PIC X(9)    VALUE SPACE.                 
026601   03 FILLER                     PIC X       VALUE '/'.                   
026701   03 WS-IDPERSON-BUY-TF         PIC X(3)    VALUE SPACE.                 
026801   03 FILLER                     PIC X       VALUE '/'.                   
026901   03 WS-IDLEVNR-TF              PIC X(5)    VALUE SPACE.                 
027001   03 FILLER                     PIC X       VALUE '/'.                   
027101   03 WS-IDTYPE-TF               PIC X       VALUE SPACE.                 
027201   03 FILLER                     PIC X       VALUE '/'.                   
027301   03 WS-STATUS-TF               PIC X       VALUE SPACE.                 
027401   03 FILLER                     PIC X       VALUE '/'.                   
027501   03 WS-TRAEFF-TF               PIC X       VALUE SPACE.                 
027601   03 FILLER                     PIC X       VALUE '/'.                   
027701   03 WS-ANTAL-TF                PIC X(7)    VALUE SPACE.                 
027801                                                                          
027901 01   WS-TESTFAELT-2.                                                     
028001   03 WS-TIVV-1-TF2              PIC 9(2)    VALUE ZERO.                  
028101   03 FILLER                     PIC X       VALUE '/'.                   
028201   03 WS-KVOI-1-TF2              PIC 9(2)    VALUE ZERO.                  
028301   03 FILLER                     PIC X       VALUE '/'.                   
028401   03 WS-TIVV-2-TF2              PIC 9(2)    VALUE ZERO.                  
028501   03 FILLER                     PIC X       VALUE '/'.                   
028601   03 WS-KVOI-2-TF2              PIC 9(2)    VALUE ZERO.                  
028701   03 FILLER                     PIC X       VALUE '/'.                   
028801   03 WS-TIVV-3-TF2              PIC 9(2)    VALUE ZERO.                  
028901   03 FILLER                     PIC X       VALUE '/'.                   
029001   03 WS-KVOI-3-TF2              PIC 9(2)    VALUE ZERO.                  
029101   03 FILLER                     PIC X       VALUE '/'.                   
029201   03 WS-TIVV-4-TF2              PIC 9(2)    VALUE ZERO.                  
029301   03 FILLER                     PIC X       VALUE '/'.                   
029401   03 WS-KVOI-4-TF2              PIC 9(2)    VALUE ZERO.                  
029501   03 FILLER                     PIC X       VALUE '/'.                   
029601   03 WS-TIVV-5-TF2              PIC 9(2)    VALUE ZERO.                  
029701   03 FILLER                     PIC X       VALUE '/'.                   
029801   03 WS-KVOI-5-TF2              PIC 9(2)    VALUE ZERO.                  
029901   03 FILLER                     PIC X       VALUE '/'.                   
030001   03 WS-KVOI-SUM-TF2            PIC 9(2)    VALUE ZERO.                  
030101   03 FILLER                     PIC X       VALUE '/'.                   
030201   03 WS-ANTAL-DC21-TF2          PIC 9(1)    VALUE ZERO.                  
030301   03 FILLER                     PIC X       VALUE '/'.                   
030401   03 WS-ANTAL-DC23-TF2          PIC 9(1)    VALUE ZERO.                  
030501   03 FILLER                     PIC X       VALUE '/'.                   
030601   03 WS-ANTAL-DC24-TF2          PIC 9(1)    VALUE ZERO.                  
030701   03 FILLER                     PIC X       VALUE '/'.                   
030801   03 WS-ANTAL-DC25-TF2          PIC 9(1)    VALUE ZERO.                  
030901   03 FILLER                     PIC X       VALUE '/'.                   
031001   03 WS-ANTAL-DC26-TF2          PIC 9(1)    VALUE ZERO.                  
031101   03 FILLER                     PIC X       VALUE '/'.                   
031201   03 WS-ANTAL-DC61-TF2          PIC 9(1)    VALUE ZERO.                  
031301   03 FILLER                     PIC X       VALUE '/'.                   
031401   03 WS-ANTAL-DC62-TF2          PIC 9(1)    VALUE ZERO.                  
031501                                                                          
031601 01   WS-TESTFAELT-3.                                                     
031701   03 WS-KR-VIKT-TF3             PIC 9(9)    VALUE ZERO.                  
031801   03 FILLER                     PIC X       VALUE '/'.                   
031901   03 WS-KR-VOLYM-TF3            PIC 9(9)    VALUE ZERO.                  
032001                                                                          
032101 01   WS-TESTFAELT-4.                                                     
032201   03 WS-FL1                     PIC X       VALUE SPACE.                 
032301   03 FILLER                     PIC X       VALUE '/'.                   
032401   03 WS-FL2                     PIC X       VALUE SPACE.                 
032501   03 FILLER                     PIC X       VALUE '/'.                   
032601   03 WS-FL3                     PIC X       VALUE SPACE.                 
032701   03 FILLER                     PIC X       VALUE '/'.                   
032801   03 WS-FL4                     PIC X       VALUE SPACE.                 
032901   03 FILLER                     PIC X       VALUE '/'.                   
033001   03 WS-FL5                     PIC X       VALUE SPACE.                 
033101   03 FILLER                     PIC X       VALUE '/'.                   
033201   03 WS-FL6                     PIC X       VALUE SPACE.                 
033301   03 FILLER                     PIC X       VALUE '/'.                   
033401   03 WS-FL7                     PIC X       VALUE SPACE.                 
033501   03 FILLER                     PIC X       VALUE '/'.                   
033601   03 WS-FL8                     PIC X       VALUE SPACE.                 
033701                                                                          
033801 01   WS-TESTFAELT-5.                                                     
033901   03 WS-STOCK-5                 PIC 9(5)    VALUE ZERO.                  
034001   03 FILLER                     PIC X       VALUE '/'.                   
034101   03 WS-KVAKS-5                 PIC 9(5)    VALUE ZERO.                  
034201   03 FILLER                     PIC X       VALUE '/'.                   
034301   03 WS-ORDERED-5               PIC 9(5)    VALUE ZERO.                  
034401   03 FILLER                     PIC X       VALUE '/'.                   
034501   03 WS-PURCHQTY-5              PIC 9(5)    VALUE ZERO.                  
034601   03 FILLER                     PIC X       VALUE '/'.                   
034701   03 WS-KVPB-REF-5              PIC 9(5)    VALUE ZERO.                  
034801                                                                          
034901 01   WS-TESTFAELT-6.                                                     
035001   03 WS-FL1-6                   PIC X       VALUE SPACE.                 
035101   03 FILLER                     PIC X       VALUE '/'.                   
035201   03 WS-FL2-6                   PIC X       VALUE SPACE.                 
035301   03 FILLER                     PIC X       VALUE '/'.                   
035401   03 WS-FL3-6                   PIC X       VALUE SPACE.                 
035501   03 FILLER                     PIC X       VALUE '/'.                   
035601   03 WS-FL4-6                   PIC X       VALUE SPACE.                 
035701   03 FILLER                     PIC X       VALUE '/'.                   
035801   03 WS-FL5-6                   PIC X       VALUE SPACE.                 
035901   03 FILLER                     PIC X       VALUE '/'.                   
036001   03 WS-FL6-6                   PIC X       VALUE SPACE.                 
036101   03 FILLER                     PIC X       VALUE '/'.                   
036201   03 WS-FL7-6                   PIC X       VALUE SPACE.                 
036301   03 FILLER                     PIC X       VALUE '/'.                   
036401   03 WS-FL8-6                   PIC X       VALUE SPACE.                 
036501   03 FILLER                     PIC X       VALUE '/'.                   
036601                                                                          
036701 01   WS-TESTFAELT-7.                                                     
036801   03 WS-TESTNING                PIC X(55)   VALUE SPACE.                 
036901                                                                          
037001 01   WS-IDDC-CDC                PIC X(2)    VALUE '11'.                  
037101 01   WS-IDDC-REF                PIC X(2)    VALUE SPACES.                
037201 01   WS-IDDC-REF-K7             PIC X(2)    VALUE SPACES.                
037301 01   WS-TIERSDAT-VIPS           PIC 9(5)    VALUE ZEROES.                
037401 01   WS-KDERS-INOM              PIC 9(3)    VALUE ZEROES.                
037501 01   WS-TIERSDAT-PREL           PIC 9(5)    VALUE ZEROES.                
037601 01   FILLER REDEFINES WS-TIERSDAT-PREL.                                  
037701   03 WS-TIERSDAT-AAVV           PIC 9(4).                                
037801   03 WS-TIERSDAT-DAG            PIC 9(1).                                
037901                                                                          
038001 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
038101     88  SW-TRAEFF-JA                        VALUE 'J'.                   
038201     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
038301                                                                          
038401 77  SW-SEASON                   PIC X       VALUE ' '.                   
038501     88  SW-SEASON-JA                        VALUE 'J'.                   
038601     88  SW-SEASON-NEJ                       VALUE 'N'.                   
038701                                                                          
038801 77  SW-KTRL-ERS                 PIC X       VALUE ' '.                   
038901     88  SW-KTRL-ERS-JA                      VALUE 'J'.                   
039001     88  SW-KTRL-ERS-NEJ                     VALUE 'N'.                   
039101                                                                          
039201 77  SW-KVPB-SEP                 PIC X       VALUE ' '.                   
039301     88  SW-KVPB-SEP-JA                      VALUE 'J'.                   
039401     88  SW-KVPB-SEP-NEJ                     VALUE 'N'.                   
039501                                                                          
039601 77  SW-HAEMTA-INPUT-FAELT       PIC X       VALUE 'J'.                   
039701     88  SW-HAEMTA-INPUT-FAELT-JA            VALUE 'J'.                   
039801     88  SW-HAEMTA-INPUT-FAELT-NEJ           VALUE 'N'.                   
039901                                                                          
040001 77  SW-ALW-AIR-SAME             PIC X       VALUE 'J'.                   
040101     88  SW-ALW-AIR-SAME-JA                  VALUE 'J'.                   
040201     88  SW-ALW-AIR-SAME-NEJ                 VALUE 'N'.                   
040301                                                                          
040401 77  SW-AUTO-REF-SAME            PIC X       VALUE 'J'.                   
040501     88  SW-AUTO-REF-SAME-JA                 VALUE 'J'.                   
040601     88  SW-AUTO-REF-SAME-NEJ                VALUE 'N'.                   
040701                                                                          
040801 77  SW-COMMENT-SAME             PIC X       VALUE 'J'.                   
040901     88  SW-COMMENT-SAME-JA                  VALUE 'J'.                   
041001     88  SW-COMMENT-SAME-NEJ                 VALUE 'N'.                   
041101                                                                          
041201 77  SW-WDK629-STATUS            PIC X       VALUE 'J'.                   
041301     88  WDK629-FINNS                        VALUE 'J'.                   
041401     88  WDK629-SAKNAS                       VALUE 'N'.                   
041501                                                                          
041601 77  INDATA-SW                   PIC X       VALUE 'J'.                   
041701     88  INDATA-OK                           VALUE 'J'.                   
041801     88  INDATA-FEL                          VALUE 'N'.                   
041901                                                                          
042001 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
042101     88  NYCKLAR-OK                          VALUE 'J'.                   
042201     88  NYCKLAR-FEL                         VALUE 'N'.                   
042301                                                                          
042401 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
042501     88  EGEN-MID                            VALUE '2392'.                
042601     88  GODK-MID                            VALUE '2391' '2352'          
042701                                                   '2353' '2354'          
042801                                                   '2355' '2356'          
042901                                                   '2357' '2358'          
043001                                                   '2359' '6322'.         
043101     88  HELP-MID                            VALUE '0551'.                
043201                                                                          
043301 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
043401     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
043501     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
043601     SKIP3                                                                
043701 77  REFILL-PART-SW              PIC X       VALUE SPACE.                 
043801     88  REFILL-PART                         VALUE 'J'.                   
043901     88  NOT-REFILL-PART                     VALUE 'N'.                   
044001     SKIP3                                                                
044101 77  REFILL-ALLOWED-SW           PIC X       VALUE SPACE.                 
044201     88  REFILL-ALLOWED-JA                   VALUE 'J'.                   
044301     88  REFILL-ALLOWED-NEJ                  VALUE 'N'.                   
044401     SKIP3                                                                
044501 77  EXT-SUPPL-CHECK-SW          PIC X       VALUE 'N'.                   
044601     88  EXT-SUPPL-CHECK-JA                  VALUE 'J'.                   
044701     88  EXT-SUPPL-CHECK-NEJ                 VALUE 'N'.                   
044801     SKIP3                                                                
044901 77  DISTRICT-FOUND-SW           PIC X       VALUE 'J'.                   
045001     88  DISTRICT-FOUND                      VALUE 'J'.                   
045101     88  DISTRICT-NOT-FOUND                  VALUE 'N'.                   
045201     EJECT                                                                
045301 01  FILLER                      PIC X(16) VALUE 'REFILLFÖRSLAG'.         
045401     SKIP3                                                                
045501*01  -COPY W271RTXT                                                       
045601     EJECT                                                                
045701*01  -COPY WWDCLAND                                                       
045801*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
045901 01  GENERELLA-SUBPROGRAM.                                                
046001     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
046101     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
046201     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
046301     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
046401     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
046501     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
046601     03  W272REFL                PIC X(8)    VALUE 'W272REFL'.            
046701     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
046801     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
046901     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
047001     03  W272UTUP                PIC X(8)    VALUE 'W272UTUP'.            
047101     EJECT                                                                
047201*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
047301*01 -COPY WMEDAREA                                                        
047401     EJECT                                                                
047501*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
047601*01  -COPY WDECAREA                                                       
047701     EJECT                                                                
047801*    --- PARAMETRAR TILL W272REFL                                         
047901*01 -COPY W272REFL                                                        
048001     EJECT                                                                
048101*    --- PARAMETRAR TILL WZ20DAYS                                         
048201*01 -COPY WZ20DAYS                                                        
048301     EJECT                                                                
048401*    --- PARAMETRAR TILL W271UTIL                                         
048501*01 -COPY W271UTIL                                                        
048601     EJECT                                                                
048701*    --- PARAMETRAR TILL W272UTUP                                         
048801*01 -COPY W272UTUP       -PRE W272-                                       
048901     EJECT                                                                
049001                                                                          
049101 01  MESSAGE-CODES.                                                       
049201     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
049301     03  CONFLICT                PIC X(3)    VALUE '002'.                 
049401     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
049501     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
049601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
049701     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
049801     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
049901     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
050001     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
050101     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
050201     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
050301     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
050401     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
050501     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
050601     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
050701     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
050801     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
050802     03  ERR-HIGH-AIR-COST       PIC X(3)    VALUE '369'.                 
050901     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
051001     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
051101     03  ERR-NOT-REFILL-PART     PIC X(3)    VALUE '957'.                 
051201                                                                          
051301 01  MEDDELANDE.                                                          
051401     03  MED-1                  PIC X(30)                                 
051501         VALUE 'TYPE : A,B OR C               '.                          
051601     03  MED-2                  PIC X(30)                                 
051701         VALUE 'STATUS : R OR N               '.                          
051801     03  MED-3                  PIC X(30)                                 
051901         VALUE 'FORECAST WRONG                '.                          
052001     03  MED-4                  PIC X(30)                                 
052101         VALUE 'PURCHQTY WRONG                '.                          
052201     03  MED-5                  PIC X(30)                                 
052301         VALUE 'AUT REFILL ORDERING WRONG     '.                          
052401     03  MED-6                  PIC X(30)                                 
052501         VALUE 'CAN NOT UPDATE WITH NEW KEY   '.                          
052601     03  MED-7                  PIC X(30)                                 
052701         VALUE 'OT: A,B OR C                  '.                          
052801     03  MED-8                  PIC X(30)                                 
052901         VALUE 'CONFLICT OT/VENDOR            '.                          
053001     03  MED-9                  PIC X(30)                                 
053101         VALUE 'NO VALID PRICE                '.                          
053201     03  MED-10                 PIC X(30)                                 
053301         VALUE 'UNEVEN MULTIPEL OF Q1         '.                          
053401     03  MED-11                 PIC X(30)                                 
053501         VALUE 'PART MARKED AS AIRFREIGHT ONLY'.                          
053601     03  MED-12                 PIC X(30)                                 
053701         VALUE 'DC 62 ONLY                    '.                          
053801     03  MED-13                 PIC X(30)                                 
053901         VALUE 'ENTER N, S OR J/Y             '.                          
054001     03  MED-14                 PIC X(30)                                 
054101         VALUE 'NOT ALLOWED FOR LOCAL PARTS   '.                          
054201     03  MED-15                 PIC X(30)                                 
054301         VALUE 'DISTRICT NOT FOUND            '.                          
054401     03  MED-16                 PIC X(30)                                 
054501         VALUE 'PART CANNOT BE REFILLED       '.                          
054601     03  MED-17                 PIC X(30)                                 
054701         VALUE 'PART CANNOT BE AIRFREIGHT ONLY'.                          
054702     03  MED-18                 PIC X(30)                                 
054703         VALUE 'PART LOCKED FOR AIRFREIGHT    '.                          
054704     03  MED-19                 PIC X(30)                                 
054705         VALUE 'WRONG REFILLING DC            '.                          
054801                                                                          
054901     EJECT                                                                
055001*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
055101*01  -COPY WDATAREA                                                       
055201     EJECT                                                                
055301*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
055401*                                                                         
055501 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
055601     SKIP3                                                                
055701*01 -COPY WMSGINIT                                                        
055801     EJECT                                                                
055901*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
056001*                                                                         
056101 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
056201     SKIP3                                                                
056301*01  MID -COPY W2I39201                                                   
056401     EJECT                                                                
056501*    --- VID HOPP FRÅN 2391 ANVÄNDS W2I39101                              
056601*    ---                                                                  
056701*01  MID -COPY W2I39101                                                   
056801     EJECT                                                                
056901 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
057001     SKIP3                                                                
057101*01  -COPY WMSGAREA                                                       
057201     EJECT                                                                
057301     03  MOD REDEFINES MSG-AREA.                                          
057401*      05  -COPY W2O39201                                                 
057501     EJECT                                                                
057601*    --- PARAMETRAR TILL SUBPROGRAM W222PBTO                              
057701*                                                                         
057801 01  FILLER                      PIC X(16)   VALUE 'W222PBTO'.            
057901     SKIP3                                                                
058001*01 -COPY W222PBTO                                                        
058101     SKIP3                                                                
058201     EJECT                                                                
058301 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
058401     SKIP3                                                                
058501*01  -COPY WMFSAREA                                                       
058601     EJECT                                                                
058701*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
058801*                                                                         
058901     EJECT                                                                
059001 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
059101     SKIP3                                                                
059201                                                                          
059301 01  NYCKLAR-TILL-DLI.                                                    
059401     03  W-IDARTNR-X.                                                     
059501         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
059601     03  W-KDSEGKEY-X.                                                    
059701         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
059801     03  W-IDDC-REF-X.                                                    
059901         05  W-IDDC-REF          PIC X(02)   VALUE SPACE.                 
060001     03  W-IDDC-X.                                                        
060101         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
060201     03  W-IDLAND-X.                                                      
060301         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
060401     03  W-IDDC-B6-X.                                                     
060501         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
060601     03  W-IDDC-B616-X.                                                   
060701         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
060801     03  W-IDUSER-X.                                                      
060901         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
061001     03  W-IDSKYLT-X.                                                     
061101         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
061201     03  W-IDLEVNR-21-X.                                                  
061301         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
061401     03  W-DAPRLIST-21-N.                                                 
061501         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
061601     03  W-IDLEVNR-K7-X.                                                  
061701         05  W-IDLEVNR-K7        PIC X(5)    VALUE LOW-VALUE.             
061801     03  W-DAPRLIST-K7-N.                                                 
061901         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
062001     03  W-TIAAAA-X.                                                      
062101         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
062201                                                                          
062301     03  W-KDNOTTYP-X.                                                    
062401         05  W-KDNOTTYP          PIC  S9(01) COMP-3                       
062501                                             VALUE ZERO.                  
062601                                                                          
062701     03 W-WDE301KY-X.                                                     
062801         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
062901         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
063001         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
063101         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
063201         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
063301                                                                          
063401     03 W-WDE301KY-MIN-X.                                                 
063501         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
063601         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
063701         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
063801         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
063901         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
064001                                                                          
064101     03 W-WDE301KY-MAX-X.                                                 
064201         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
064301         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
064401         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
064501         05  W-IDARTNR-MAX       PIC S9(9)                                
064601                                         VALUE +999999999 COMP-3.         
064701         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
064801                                                                          
064802     03 W-IDDC-REF-MIN-X.                                                 
064803         05 W-IDDC-REF-MIN       PIC X(2)  VALUE SPACE.                   
064804                                                                          
064805     03 W-IDDC-REF-MAX-X.                                                 
064806         05 W-IDDC-REF-MAX       PIC X(2)  VALUE HIGH-VALUE.              
064807                                                                          
064901     03  W-WDA5A1KY-MIN.                                                  
065001         05  W-IDARTNR-N3-MIN     PIC S9(9)  VALUE ZERO COMP-3.           
065101         05  W-IDDC-N3-MIN        PIC X(2)   VALUE SPACE.                 
065201         05  FILLER               PIC X(33)  VALUE LOW-VALUE.             
065301                                                                          
065401     03  W-WDA5A1KY-MAX.                                                  
065501         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
065601         05  W-IDDC-N3-MAX        PIC X(2)           VALUE SPACE.         
065701         05  FILLER               PIC X(33)  VALUE HIGH-VALUE.            
065801                                                                          
064901     03  W-WDA5A2KY-MIN.                                                  
065001         05  W-IDARTNR-N4-MIN     PIC S9(9)  COMP-3 VALUE ZERO.           
065201         05  FILLER               PIC X(35)  VALUE LOW-VALUE.             
065301                                                                          
065401     03  W-WDA5A2KY-MAX.                                                  
065501         05  W-IDARTNR-N4-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
065701         05  FILLER               PIC X(35)  VALUE HIGH-VALUE.            
065801                                                                          
055300                                                                          
055400     03  W-WDA501KY.                                                      
055500         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
055600         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
055700         05  W-IDKUNDRF-N2.                                               
055800             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
055900             07  FILLER           PIC X(5)           VALUE SPACE.         
056000         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
056100         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
056200                                                                          
065901     03  W-IDDISTR-REF-X.                                                 
066001         05  W-IDDISTR-REF        PIC S9(5) COMP-3   VALUE +9111.         
066101     03  W-KDORDKL-1-X.                                                   
066201         05  W-KDORDKL-1          PIC S9    COMP-3   VALUE +1.            
066301     03  W-KDSTARAD-2-X.                                                  
066401         05  W-KDSTARAD-2         PIC X              VALUE '2'.           
066501                                                                          
066601   03  W-WDN611KY-X.                                                      
066701     05  W-IDFORDON              PIC S9(2)   VALUE ZERO  COMP-3.          
066801     05  W-TIOMBRYT-1            PIC S9(7)   VALUE ZERO  COMP-3.          
066901     SKIP2                                                                
067001                                                                          
067101   03  W-WDD7A1KY-MIN.                                                    
067201     05  W-IDARTNR-MIN7           PIC S9(9)  COMP-3 VALUE ZERO.           
067301     05  FILLER                   PIC S9(9)  COMP-3 VALUE ZERO.           
067401     05  FILLER                   PIC S9(3)  COMP-3 VALUE ZERO.           
067501                                                                          
067601   03  W-WDD7A1KY-MAX.                                                    
067701     05  W-IDARTNR-MAX7    PIC S9(9)  COMP-3 VALUE ZERO.                  
067801     05  FILLER            PIC S9(9)  COMP-3 VALUE +999999999.            
067901     05  FILLER            PIC S9(3)  COMP-3 VALUE +999.                  
068001                                                                          
068101   03  W-IDARTNR-TILLK-X.                                                 
068201     05  W-IDARTNR-TILLK         PIC S9(9)   COMP-3.                      
068301   03  W-IDARTNR-ERS-LOW-X.                                               
068401     05  W-IDARTNR-ERS-LOW       PIC S9(9)   COMP-3  VALUE ZERO.          
068501   03  W-IDARTNR-ERS-HIGH-X.                                              
068601     05  W-IDARTNR-ERS-HIGH      PIC S9(9)   COMP-3                       
068701                                  VALUE +999999999.                       
068801                                                                          
068901   03  W-W6D1HSEQ-X.                                                      
069001     05  W-IDARTNR-HSEQ  PIC S9(9)   VALUE ZERO  COMP-3.                  
069101                                                                          
069201   03  W-IDLEVNR-X.                                                       
069301         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
069401                                                                          
069501     SKIP2                                                                
069601*    --- STATUS-KOD FRÅN IMS                                              
069701 01  STATUS-WS                   PIC XX.                                  
069801     88  SEGMENT-FINNS                       VALUE '  '.                  
069901     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
070001     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
070101     88  SEGMENT-SLUT                        VALUE 'GB'.                  
070201                                                                          
070301     SKIP2                                                                
070401 01  GODK-STATUSKODER.                                                    
070501     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
070601     SKIP3                                                                
070701 01  SSA1                        PIC X(256).                              
070801 01  SSA2                        PIC X(128).                              
070901 01  SSA3                        PIC X(128).                              
071001     EJECT                                                                
071101*01  -COPY WWDCKONS                                                       
071201     EJECT                                                                
071301*    --- IMS FUNKTIONSKODER                                               
071401*01  -COPY W0003                                                          
071501     EJECT                                                                
071601*    ---  DLI INPUT-OUTPUT AREA                                           
071701 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
071801     SKIP3                                                                
071901 01  DLI-IO-AREA-WDK711.                                                  
072001*    03  -COPY WDK711                                                     
072101     EJECT                                                                
072201 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
072301     SKIP3                                                                
072401 01  DLI-IO-AREA-WDK712.                                                  
072501*    03  -COPY WDK712                                                     
072601     EJECT                                                                
072701 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
072801     SKIP3                                                                
072901 01  DLI-IO-AREA-WDK722.                                                  
073001*    03  -COPY WDK722                                                     
073101     EJECT                                                                
073201 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
073301     SKIP3                                                                
073401 01  DLI-IO-AREA-WDL811.                                                  
073501*    03  -COPY WDL811                                                     
073601     EJECT                                                                
073701 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
073801     SKIP3                                                                
073901 01  DLI-IO-AREA-WDD301.                                                  
074001*    03  -COPY WDD301  -PRE WDD301-                                       
074101     EJECT                                                                
074201 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
074301     SKIP3                                                                
074401 01  DLI-IO-AREA-WDD311.                                                  
074501*    03  -COPY WDD311  -PRE WDD311-                                       
074601     EJECT                                                                
074701 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301'.             
074801     SKIP3                                                                
074901 01  DLI-IO-AREA-WDE301.                                                  
075001*    03  -COPY WDE301                                                     
075101     EJECT                                                                
075201 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDA5A1'.             
075301 01  DLI-IO-AREA-WDA5A1.                                                  
075401*    03  WDA5A1 -COPY WDA5A1                                              
075501     EJECT                                                                
066100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDA501'.             
066200 01  DLI-IO-AREA-WDA501.                                                  
066300*    03  WDA501 -COPY WDA501                                              
075701     EJECT                                                                
075601 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
075801 01  DLI-IO-AREA-WDK601.                                                  
075901*    03  -COPY WDK601                                                     
076001     EJECT                                                                
076101 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
076201     SKIP3                                                                
076301 01  DLI-IO-AREA-WDK611.                                                  
076401*    03  -COPY WDK611                                                     
076501     EJECT                                                                
076601 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK625'.             
076701     SKIP3                                                                
076801 01  DLI-IO-AREA-WDK625.                                                  
076901*    03  -COPY WDK625                                                     
077001     EJECT                                                                
077101 01  DLI-IO-AREA-WDK626.                                                  
077201*    03  -COPY WDK626                                                     
077301     EJECT                                                                
077401 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK629'.             
077501     SKIP3                                                                
077601 01  DLI-IO-AREA-WDK629.                                                  
077701*    03  -COPY WDK629                                                     
077801 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK61129'.           
077901     SKIP3                                                                
078001 01  DLI-IO-AREA-WDK61129.                                                
078101*    03  -COPY WDK611 -PRE K6-                                            
078201*    03  -COPY WDK629 -PRE K6-                                            
078301     EJECT                                                                
078401 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN601'.            
078501     SKIP3                                                                
078601 01  DLI-IO-AREA-WDN601.                                                  
078701*    03  -COPY WDN601 -PRE K6-                                            
078801     EJECT                                                                
078901 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDN611'.            
079001     SKIP3                                                                
079101 01  DLI-IO-AREA-WDN611.                                                  
079201*    03  -COPY WDN611 -PRE WDN6-                                          
079301     EJECT                                                                
079401 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD701'.            
079501     SKIP3                                                                
079601 01  DLI-IO-AREA-WDD701.                                                  
079701*    03  -COPY WDD701  -PRE WDD701-                                       
079801     EJECT                                                                
079901 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD702'.            
080001     SKIP3                                                                
080101 01  DLI-IO-AREA-WDD702.                                                  
080201*    03  -COPY WDD702  -PRE WDD702-                                       
080301     EJECT                                                                
080401 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD704'.            
080501     SKIP3                                                                
080601 01  DLI-IO-AREA-WDD704.                                                  
080701*    03  -COPY WDD704  -PRE WDD704-                                       
080801     EJECT                                                                
080901 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD7A1'.            
081001     SKIP3                                                                
081101 01  DLI-IO-AREA-WDD7A1.                                                  
081201*    03  -COPY WDD7A1  -PRE WDD7A1-                                       
081301     EJECT                                                                
081401                                                                          
081501 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-W6D111'.            
081601     SKIP3                                                                
081701 01  DLI-IO-AREA-W6D111.                                                  
081801*    03  -COPY W6D111 -PRE W6D1-                                          
081901     EJECT                                                                
082001                                                                          
082101 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL601'.            
082201 01  DLI-IO-AREA-WDL601.                                                  
082301*    03  -COPY WDL601 -PRE WDL6-                                          
082401     EJECT                                                                
082501                                                                          
082601 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDL611'.            
082701 01  DLI-IO-AREA-WDL611.                                                  
082801*    03  -COPY WDL611 -PRE WDL6-                                          
082901     EJECT                                                                
083001                                                                          
083101 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDK901'.            
083201     SKIP3                                                                
083301 01  DLI-IO-AREA-WDK901.                                                  
083401*    03  -COPY WDK901  -PRE WDK9-                                         
083501     EJECT                                                                
083601                                                                          
083701 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
083801 01  DLI-IO-AREA-B601.                                                    
083901*    03  -COPY WDB601                                                     
084001 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
084101 01  DLI-IO-AREA-B616.                                                    
084201*    03  -COPY WDB616 -PRE B6-                                            
084301     EJECT                                                                
084401                                                                          
084501 LINKAGE SECTION.                                                         
084601                                                                          
084701*01  -COPY W0009   -PRE MSG-                                              
084801     EJECT                                                                
084901*01  -COPY W0008  -PRE  USEA-                                             
085001     05  FILLER                  PIC X.                                   
085101     EJECT                                                                
085201*01  -COPY W0008  -PRE  WDK7-                                             
085301     05  FILLER                  PIC X.                                   
085401     EJECT                                                                
085501*01  -COPY W0008  -PRE  WDL8-                                             
085601     05  FILLER                  PIC X.                                   
085701     EJECT                                                                
085801*01  -COPY W0008  -PRE  WDD3-                                             
085901     05  FILLER                  PIC X.                                   
086001     EJECT                                                                
086101*01  -COPY W0008  -PRE WDE3-                                              
086201     05  FILLER                  PIC X.                                   
086301     EJECT                                                                
086401*01  -COPY W0008  -PRE WDA5A-                                             
086501     05  FILLER                  PIC X.                                   
086401*01  -COPY W0008  -PRE WDA5-                                              
086501     05  FILLER                  PIC X.                                   
086601     EJECT                                                                
086701*01  -COPY W0008  -PRE WDK6-                                              
086801     05  FILLER                  PIC X.                                   
086901     EJECT                                                                
087001*01  -COPY W0008  -PRE WDN6-                                              
087101     05  FILLER                  PIC X.                                   
087201     EJECT                                                                
087301*01  -COPY W0008  -PRE WDD7-                                              
087401     05  FILLER                  PIC X.                                   
087501     EJECT                                                                
087601*01  -COPY W0008  -PRE WDD7A-                                             
087701     05  FILLER                  PIC X.                                   
087801     EJECT                                                                
087901*01  -COPY W0008  -PRE W6D1-                                              
088001     05  FILLER                  PIC X.                                   
088101     EJECT                                                                
088201*01  -COPY W0008  -PRE WDK9-                                              
088301     05  FILLER                  PIC X.                                   
088401     EJECT                                                                
088501*01  -COPY W0008  -PRE WDL6-                                              
088601     05  FILLER                  PIC X.                                   
088701     EJECT                                                                
088801*01  -COPY W0008  -PRE WDB6-                                              
088901     05  FILLER                  PIC X.                                   
089001     EJECT                                                                
089101*****W222PBTO**********                                                   
089201 01  PBTO-WDK6-PCB                 PIC X.                                 
089301 01  PBTO-WDK7-PCB                 PIC X.                                 
089401 01  PBTO-ARTM-PCB                 PIC X.                                 
089501 01  PBTO-2501-PCB                 PIC X.                                 
089601 01  PBTO-WDB6R-PCB                PIC X.                                 
089701 01  PBTO-WDK7R-PCB                PIC X.                                 
089801 01  PBTO-WDB6-PCB                 PIC X.                                 
089901 01  PBTO-WDD7-PCB                 PIC X.                                 
090001 01  PBTO-WDK7E-PCB                PIC X.                                 
090101 01  PBTO-W222-UTIL-WDK6-PCB       PIC X.                                 
090201 01  PBTO-W222-UTIL-WDK7-PCB       PIC X.                                 
090301 01  PBTO-W222-UTIL-WDB6-PCB       PIC X.                                 
090401 01  PBTO-W222-UTUP-WDK7-PCB       PIC X.                                 
090501 01  PBTO-W222-UTUP-WDB6-PCB       PIC X.                                 
090601 01  PBTO-W222-UTUP-UTIL-WDK6-PCB  PIC X.                                 
090701 01  PBTO-W222-UTUP-UTIL-WDK7-PCB  PIC X.                                 
090801 01  PBTO-W222-UTUP-UTIL-WDB6-PCB  PIC X.                                 
090901*****W271UTIL**********                                                   
091001 01  UTIL-WDK6-PCB                 PIC X.                                 
091101 01  UTIL-WDK7-PCB                 PIC X.                                 
091201 01  UTIL-WDB6-PCB                 PIC X.                                 
091301*****W272REFL**********                                                   
091401 01  REFL2-2501-PCB                PIC X.                                 
091501 01  REFL2-WDB6-PCB                PIC X.                                 
091601 01  REFL2-UTIL-WDK6-PCB           PIC X.                                 
091701 01  REFL2-UTIL-WDK7-PCB           PIC X.                                 
091801 01  REFL2-UTIL-WDB6-PCB           PIC X.                                 
091901*****W272UTUP**********                                                   
092001 01  U2-WDK6-PCB                         PIC X.                           
092101 01  U2-WDB6-PCB                         PIC X.                           
092201 01  U2-PBTO-W222-WDK6-PCB               PIC X.                           
092301 01  U2-PBTO-W222-WDK7-PCB               PIC X.                           
092401 01  U2-PBTO-W222-ARTM-PCB               PIC X.                           
092501 01  U2-PBTO-W222-REFL1-2501-PCB         PIC X.                           
092601 01  U2-PBTO-W222-REFL1-WDB6R-PCB        PIC X.                           
092701 01  U2-PBTO-W222-REFL1-WDK7R-PCB        PIC X.                           
092801 01  U2-PBTO-W222-REFL1-UTIL-K6-PCB      PIC X.                           
092901 01  U2-PBTO-W222-REFL1-UTIL-K7-PCB      PIC X.                           
093001 01  U2-PBTO-W222-REFL1-UTIL-B6-PCB      PIC X.                           
093101 01  U2-PBTO-W222-WDB6-PCB               PIC X.                           
093201 01  U2-PBTO-W222-WDD7-PCB               PIC X.                           
093301 01  U2-PBTO-W222-WDK7E-PCB              PIC X.                           
093401 01  U2-PBTO-W222-UTUP1-WDK7-PCB         PIC X.                           
093501 01  U2-PBTO-W222-UTUP1-WDB6-PCB         PIC X.                           
093601 01  U2-PBTO-W222-UTUP1-UTIL-K6-PCB      PIC X.                           
093701 01  U2-PBTO-W222-UTUP1-UTIL-K7-PCB      PIC X.                           
093801 01  U2-PBTO-W222-UTUP1-UTIL-B6-PCB      PIC X.                           
093901 01  U2-REFL2-2501-PCB                   PIC X.                           
094001 01  U2-REFL2-WDB6-PCB                   PIC X.                           
094101 01  U2-REFL2-UTIL-WDK6-PCB              PIC X.                           
094201 01  U2-REFL2-UTIL-WDK7-PCB              PIC X.                           
094301 01  U2-REFL2-UTIL-WDB6-PCB              PIC X.                           
094401 01  U2-W222-WDK6-PCB                    PIC X.                           
094501 01  U2-W222-WDK7-PCB                    PIC X.                           
094601 01  U2-W222-ARTM-PCB                    PIC X.                           
094701 01  U2-W222-2501-PCB                    PIC X.                           
094801 01  U2-W222-WDB6R-PCB                   PIC X.                           
094901 01  U2-W222-WDK7R-PCB                   PIC X.                           
095001 01  U2-W222-WDB6-PCB                    PIC X.                           
095101 01  U2-W222-WDD7-PCB                    PIC X.                           
095201 01  U2-W222-WDK7E-PCB                   PIC X.                           
095301 01  U2-W222-UTIL-WDK6-PCB               PIC X.                           
095401 01  U2-W222-UTIL-WDK7-PCB               PIC X.                           
095501 01  U2-W222-UTIL-WDB6-PCB               PIC X.                           
095601 01  U2-W222-UTUP1-WDK7-PCB              PIC X.                           
095701 01  U2-W222-UTUP1-WDB6-PCB              PIC X.                           
095801 01  U2-W222-UTUP1-UTIL-WDK6-PCB         PIC X.                           
095901 01  U2-W222-UTUP1-UTIL-WDK7-PCB         PIC X.                           
096001 01  U2-W222-UTUP1-UTIL-WDB6-PCB         PIC X.                           
096101     EJECT                                                                
096201 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK7-PCB                      
096301     WDL8-PCB  WDD3-PCB WDE3-PCB WDA5A-PCB WDA5-PCB                       
096401     WDK6-PCB  WDN6-PCB WDD7-PCB WDD7A-PCB  W6D1-PCB WDK9-PCB             
096501     WDL6-PCB WDB6-PCB                                                    
096601     PBTO-WDK6-PCB                                                        
096701     PBTO-WDK7-PCB                                                        
096801     PBTO-ARTM-PCB                                                        
096901     PBTO-2501-PCB                                                        
097001     PBTO-WDB6R-PCB                                                       
097101     PBTO-WDK7R-PCB                                                       
097201     PBTO-WDB6-PCB                                                        
097301     PBTO-WDD7-PCB                                                        
097401     PBTO-WDK7E-PCB                                                       
097501     PBTO-W222-UTIL-WDK6-PCB                                              
097601     PBTO-W222-UTIL-WDK7-PCB                                              
097701     PBTO-W222-UTIL-WDB6-PCB                                              
097801     PBTO-W222-UTUP-WDK7-PCB                                              
097901     PBTO-W222-UTUP-WDB6-PCB                                              
098001     PBTO-W222-UTUP-UTIL-WDK6-PCB                                         
098101     PBTO-W222-UTUP-UTIL-WDK7-PCB                                         
098201     PBTO-W222-UTUP-UTIL-WDB6-PCB                                         
098301     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
098401     REFL2-2501-PCB                                                       
098501     REFL2-WDB6-PCB                                                       
098601     REFL2-UTIL-WDK6-PCB                                                  
098701     REFL2-UTIL-WDK7-PCB                                                  
098801     REFL2-UTIL-WDB6-PCB                                                  
098901     U2-WDK6-PCB                                                          
099001     U2-WDB6-PCB                                                          
099101     U2-PBTO-W222-WDK6-PCB                                                
099201     U2-PBTO-W222-WDK7-PCB                                                
099301     U2-PBTO-W222-ARTM-PCB                                                
099401     U2-PBTO-W222-REFL1-2501-PCB                                          
099501     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
099601     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
099701     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
099801     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
099901     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
100001     U2-PBTO-W222-WDB6-PCB                                                
100101     U2-PBTO-W222-WDD7-PCB                                                
100201     U2-PBTO-W222-WDK7E-PCB                                               
100301     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
100401     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
100501     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
100601     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
100701     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
100801     U2-REFL2-2501-PCB                                                    
100901     U2-REFL2-WDB6-PCB                                                    
101001     U2-REFL2-UTIL-WDK6-PCB                                               
101101     U2-REFL2-UTIL-WDK7-PCB                                               
101201     U2-REFL2-UTIL-WDB6-PCB                                               
101301     U2-W222-WDK6-PCB                                                     
101401     U2-W222-WDK7-PCB                                                     
101501     U2-W222-ARTM-PCB                                                     
101601     U2-W222-2501-PCB                                                     
101701     U2-W222-WDB6R-PCB                                                    
101801     U2-W222-WDK7R-PCB                                                    
101901     U2-W222-WDB6-PCB                                                     
102001     U2-W222-WDD7-PCB                                                     
102101     U2-W222-WDK7E-PCB                                                    
102201     U2-W222-UTIL-WDK6-PCB                                                
102301     U2-W222-UTIL-WDK7-PCB                                                
102401     U2-W222-UTIL-WDB6-PCB                                                
102501     U2-W222-UTUP1-WDK7-PCB                                               
102601     U2-W222-UTUP1-WDB6-PCB                                               
102701     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
102801     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
102901     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
103001 MAIN SECTION.                                                            
103101     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK7-PCB                      
103201     WDL8-PCB  WDD3-PCB WDE3-PCB WDA5A-PCB WDA5-PCB                       
103301     WDK6-PCB  WDN6-PCB WDD7-PCB WDD7A-PCB  W6D1-PCB WDK9-PCB             
103401     WDL6-PCB  WDB6-PCB                                                   
103501     PBTO-WDK6-PCB                                                        
103601     PBTO-WDK7-PCB                                                        
103701     PBTO-ARTM-PCB                                                        
103801     PBTO-2501-PCB                                                        
103901     PBTO-WDB6R-PCB                                                       
104001     PBTO-WDK7R-PCB                                                       
104101     PBTO-WDB6-PCB                                                        
104201     PBTO-WDD7-PCB                                                        
104301     PBTO-WDK7E-PCB                                                       
104401     PBTO-W222-UTIL-WDK6-PCB                                              
104501     PBTO-W222-UTIL-WDK7-PCB                                              
104601     PBTO-W222-UTIL-WDB6-PCB                                              
104701     PBTO-W222-UTUP-WDK7-PCB                                              
104801     PBTO-W222-UTUP-WDB6-PCB                                              
104901     PBTO-W222-UTUP-UTIL-WDK6-PCB                                         
105001     PBTO-W222-UTUP-UTIL-WDK7-PCB                                         
105101     PBTO-W222-UTUP-UTIL-WDB6-PCB                                         
105201     UTIL-WDK6-PCB UTIL-WDK7-PCB UTIL-WDB6-PCB                            
105301     REFL2-2501-PCB                                                       
105401     REFL2-WDB6-PCB                                                       
105501     REFL2-UTIL-WDK6-PCB                                                  
105601     REFL2-UTIL-WDK7-PCB                                                  
105701     REFL2-UTIL-WDB6-PCB                                                  
105801     U2-WDK6-PCB                                                          
105901     U2-WDB6-PCB                                                          
106001     U2-PBTO-W222-WDK6-PCB                                                
106101     U2-PBTO-W222-WDK7-PCB                                                
106201     U2-PBTO-W222-ARTM-PCB                                                
106301     U2-PBTO-W222-REFL1-2501-PCB                                          
106401     U2-PBTO-W222-REFL1-WDB6R-PCB                                         
106501     U2-PBTO-W222-REFL1-WDK7R-PCB                                         
106601     U2-PBTO-W222-REFL1-UTIL-K6-PCB                                       
106701     U2-PBTO-W222-REFL1-UTIL-K7-PCB                                       
106801     U2-PBTO-W222-REFL1-UTIL-B6-PCB                                       
106901     U2-PBTO-W222-WDB6-PCB                                                
107001     U2-PBTO-W222-WDD7-PCB                                                
107101     U2-PBTO-W222-WDK7E-PCB                                               
107201     U2-PBTO-W222-UTUP1-WDK7-PCB                                          
107301     U2-PBTO-W222-UTUP1-WDB6-PCB                                          
107401     U2-PBTO-W222-UTUP1-UTIL-K6-PCB                                       
107501     U2-PBTO-W222-UTUP1-UTIL-K7-PCB                                       
107601     U2-PBTO-W222-UTUP1-UTIL-B6-PCB                                       
107701     U2-REFL2-2501-PCB                                                    
107801     U2-REFL2-WDB6-PCB                                                    
107901     U2-REFL2-UTIL-WDK6-PCB                                               
108001     U2-REFL2-UTIL-WDK7-PCB                                               
108101     U2-REFL2-UTIL-WDB6-PCB                                               
108201     U2-W222-WDK6-PCB                                                     
108301     U2-W222-WDK7-PCB                                                     
108401     U2-W222-ARTM-PCB                                                     
108501     U2-W222-2501-PCB                                                     
108601     U2-W222-WDB6R-PCB                                                    
108701     U2-W222-WDK7R-PCB                                                    
108801     U2-W222-WDB6-PCB                                                     
108901     U2-W222-WDD7-PCB                                                     
109001     U2-W222-WDK7E-PCB                                                    
109101     U2-W222-UTIL-WDK6-PCB                                                
109201     U2-W222-UTIL-WDK7-PCB                                                
109301     U2-W222-UTIL-WDB6-PCB                                                
109401     U2-W222-UTUP1-WDK7-PCB                                               
109501     U2-W222-UTUP1-WDB6-PCB                                               
109601     U2-W222-UTUP1-UTIL-WDK6-PCB                                          
109701     U2-W222-UTUP1-UTIL-WDK7-PCB                                          
109801     U2-W222-UTUP1-UTIL-WDB6-PCB.                                         
109901                                                                          
110001     PERFORM IMS-GET-MSG                                                  
110101     IF SEGMENT-FINNS                                                     
110201       PERFORM A-INIT                                                     
110301       PERFORM B-KOLLA-NYCKLAR                                            
110401       IF NYCKLAR-OK                                                      
110501         IF MFS-UPDATE OR                                                 
110502            MFS-UPD-V                                                     
110601           PERFORM I-KOLLA-INPUT                                          
110701           IF INDATA-OK                                                   
110801             PERFORM H-UPD-WDK6                                           
110901             IF WS-IDREFTYP = 'A'                                         
111001             OR WS-IDREFTYP = 'B'                                         
111101             OR WS-IDREFTYP = 'C'                                         
111201               PERFORM N-UPD-WDE3-WDK6                                    
111301             END-IF                                                       
111401                                                                          
111501             MOVE INF-UPDATE-DONE                                         
111601                             TO MED-IDMFSINF                              
111701             CALL WMEDKONV USING MED-WMEDAREA                             
111801             MOVE MED-TEMFSINF                                            
111901                             TO MOD-TEMFSINF                              
112001           ELSE                                                           
112101*---     MOD-FÄLT WILL NOT BE RESET BY H-FAMETA-INFO                      
112201*                                                                         
112301             MOVE NEJ        TO SW-HAEMTA-INPUT-FAELT                     
112401             MOVE 'PROPOSAL NOT REVIEWED'                                 
112501                             TO MOD-ORDERSTATUS                           
112601             MOVE INF-UPDATE-NOT-DONE                                     
112701                             TO MED-IDMFSINF                              
112801             CALL WMEDKONV USING MED-WMEDAREA                             
112901             MOVE MED-TEMFSINF                                            
113001                             TO MOD-TEMFSINF                              
113101           END-IF                                                         
113201         ELSE                                                             
113301                                                                          
113401*---     INITIATE MOD-PURCHQTY                                            
113501           MOVE ZERO             TO WS-RED-PURCHQTY                       
113601           MOVE WS-RED-PURCHQTY                                           
113701                                 TO MOD-PURCHQTY                          
113901           IF MFS-FIRST                                                   
114001             PERFORM C-FOEREG-SIDA                                        
114101           ELSE                                                           
114201             IF MFS-NEXT                                                  
114301               PERFORM D-NAESTA-SIDA                                      
114401             ELSE                                                         
114501               PERFORM E-SAMMA-SIDA                                       
114601             END-IF                                                       
114701           END-IF                                                         
114801           MOVE W-IDARTNR     TO W-IDARTNR-SAVED                          
114802           MOVE WS-MSGI-IDDC-REF  TO W-IDDC-REF-SAVED                     
115101         END-IF                                                           
115201                                                                          
115301*---     REFERSH SCREEN                                                   
115401         IF W-IDARTNR > ZERO                                              
115501           PERFORM S1-SECURITY-CHECK-PARTNO                               
115601           PERFORM F-HAEMTA-INFO                                          
115701         END-IF                                                           
115801                                                                          
115901*---     CALCULATE NUMBER OF PROPOSALS TO REVIEW                          
116001         PERFORM J-FYLL-I-ANT-REVIEW                                      
116101*                                                                         
116201         IF W-IDARTNR        NOT  = W-IDARTNR-SAVED                       
116301         AND W-IDARTNR            = ZERO                                  
116401             MOVE W-IDARTNR-SAVED                                         
116501                                 TO W-IDARTNR                             
116601         END-IF                                                           
116701         IF W-IDARTNR = ZERO                                              
116801           MOVE MFS-RENSA-FAELT                                           
116901                                 TO MOD-IDARTNR-UT                        
117001                                    MOD-IDREFTYP-UT                       
117101                                    MOD-ORDERSTATUS                       
117102                                    MOD-IDDC-REF-UT                       
117201         END-IF                                                           
117301                                                                          
117401         MOVE W-IDARTNR          TO WS-MSGI-IDARTNR-ENTER                 
117501         IF MOD-ORDERSTATUS       = 'REVIEWED'                            
117601            MOVE 'R'             TO WS-MSGI-ORDER-ENTER                   
117701         ELSE                                                             
117801            MOVE 'N'             TO WS-MSGI-ORDER-ENTER                   
117901         END-IF                                                           
118001         IF WS-MSGI-IDARTNR-ENTER = WS-MSGI-IDARTNR-PF7                   
118101           MOVE ZERO             TO WS-MSGI-IDARTNR-PF7                   
118201           MOVE SPACE            TO WS-MSGI-ORDER-PF7                     
118301         END-IF                                                           
118401*---     REFERSH MSGI-SPAR-AREA                                           
118501         MOVE '002'              TO MSGI-KDCALL                           
118601         MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                     
118701         MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                           
118801         MOVE '2392'             TO MSGI-IDTRANS                          
118901         MOVE WS-MSGI-AREA-2392  TO MSGI-SPAR-AREA                        
119001         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
119101                                                                          
119201*---     UPPDATERA MSGI-IDARTNR                                           
119301         MOVE ALL '+'            TO MSGI-WMSGINIT                         
119401         MOVE '001'              TO MSGI-KDCALL                           
119501         MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                     
119601         MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                           
119701         MOVE '2392'             TO MSGI-IDTRANS                          
119801         MOVE W-IDARTNR          TO WS-IDARTNR-NUM                        
119901         MOVE WS-IDARTNR         TO MSGI-IDARTNR                          
119902         MOVE W-IDDC-REF-SAVED   TO MSGI-IDDC-REF                         
120001         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
120101                                                                          
120201       END-IF                                                             
120301                                                                          
120401       IF BLOCKED-SECURITY-CHECK                                          
120501*---     IF THE USER IS NOT GRANTED AUTHORIZATION TO VIEW                 
120601*        PART INFO                                                        
120701         PERFORM MFS-RENSA-FAELT-IN                                       
120801         PERFORM MFS-RENSA-FAELT-UT                                       
120901         MOVE MFS-RENSA-FAELT    TO MOD-PURCHQTY                          
121001                                    MOD-KVPB-SEP                          
121101                                    MOD-FLREFBEO                          
121201                                    MOD-FLAGGA-FCD                        
121301                                    MOD-COMMENT(1)                        
121401                                    MOD-COMMENT(2)                        
121501         MOVE MFS-STAENG-FAELT   TO MOD-PURCHQTY-ATTR                     
121601                                    MOD-KVPB-SEP-ATTR                     
121701                                    MOD-FLREFBEO-ATTR                     
121801                                    MOD-FLAGGA-FCD-ATTR                   
121901                                    MOD-COMMENT-ATTR(1)                   
122001                                    MOD-COMMENT-ATTR(2)                   
122101       END-IF                                                             
122201                                                                          
122301       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O39201 + 4                      
122401       PERFORM IMS-INSERT-MSG                                             
122501     END-IF                                                               
122601                                                                          
122701     MOVE ZERO TO RETURN-CODE                                             
122801     GOBACK                                                               
122901     .                                                                    
123001     EJECT                                                                
123101                                                                          
123201 A-INIT SECTION.                                                          
123301                                                                          
123401     IF MSG-DUBBLA-TRANSKODER                                             
123501       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
123601       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
123701       IF MFS-IDTRANS = '2391'                                            
123801         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
123901                             TO MID-W2I39101                              
124001       ELSE                                                               
124101         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
124201                             TO MID-W2I39201                              
124301       END-IF                                                             
124401     ELSE                                                                 
124501       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
124601       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
124701       IF MFS-IDTRANS = '2391'                                            
124801         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
124901                             TO MID-W2I39101                              
125001       ELSE                                                               
125101         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
125201                             TO MID-W2I39201                              
125301       END-IF                                                             
125401     END-IF                                                               
125501                                                                          
125601     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
125701     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
125801     MOVE MFS-IDTRANS TO W-IDTRANS                                        
125901                                                                          
126001     MOVE LOW-VALUE TO MSG-AREA                                           
126101     MOVE 'W2O392N1' TO MFS-IDMOD                                         
126201     MOVE '2392' TO MOD-IDTRANS                                           
126301     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
126401                                                                          
126501     IF EGEN-MID OR HELP-MID                                              
126601       CONTINUE                                                           
126701     ELSE                                                                 
126801       MOVE SPACE TO MFS-KDTRTYP                                          
126901     END-IF                                                               
127001                                                                          
127101     MOVE +2       TO SPRAK-IX                                            
127201     MOVE 'GB'     TO MED-IDSKYLT                                         
127301                                                                          
127401     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
127501                                                                          
127601     ACCEPT DAGENS-DATUM FROM DATE                                        
127701                                                                          
127801     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
127901     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
128001                                                                          
128101     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
128201                     DAT-O-TIDATUM DAT-KDSVAR                             
128301                                                                          
128401     IF DAT-KDSVAR-OK                                                     
128501****             GET CENTURY INFORMATION                                  
128601                                                                          
128701       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
128801       MOVE DAT-TIAARP       TO DAGENS-PER                                
128901       MOVE DAT-TIVV         TO DAGENS-VECKA                              
129001*                                                                         
129101       MOVE DAT-TIAAVVD      TO WS-TIPBDAT                                
129201                                                                          
129301     ELSE                                                                 
129401         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
129501         DELIMITED BY SIZE INTO FELTEXT                                   
129601         CALL FELLOG                                                      
129701     END-IF                                                               
129801                                                                          
129901     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
130001                                                                          
130101     MOVE 'AARP  '           TO DAT-KDDATFORM                             
130201     MOVE DAT-TIAARP         TO DAT-I-TIDATUM                             
130301                                                                          
130401     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
130501                     DAT-O-TIDATUM DAT-KDSVAR                             
130601                                                                          
130701     IF DAT-KDSVAR-OK                                                     
130801****             COUNT NUMBER OF WEEKS IN CURRENT PERIOD                  
130901                                                                          
131001       COMPUTE WS-ANTAL-VECKOR = DAGENS-VECKA - DAT-TIVV + 1              
131101                                                                          
131201     ELSE                                                                 
131301         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
131401         DELIMITED BY SIZE INTO FELTEXT                                   
131501         CALL FELLOG                                                      
131601     END-IF                                                               
131701                                                                          
131801                                                                          
131901*****   INPUT/OUTPUT FIELD INITIALIZATION                                 
132001     PERFORM MFS-LAES-IN-IGEN                                             
132101*    TEXT 'NO PROPOSAL' SKRIVS I BILDEN OM EJ TRÄFF                       
132201     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
132301     .                                                                    
132401     EJECT                                                                
132501                                                                          
132601 B-KOLLA-NYCKLAR SECTION.                                                 
132701                                                                          
132801     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
132901                                MOD-IDTYPE-IN                             
133001                                MOD-IDPERSON-BUY-IN                       
133101                                MOD-IDSTATUS-IN                           
133201                                MOD-IDREFTYP-IN                           
133202                                MOD-IDDC-REF-IN                           
133301     MOVE ALL '+'            TO MSGI-WMSGINIT                             
133401     MOVE '001'              TO MSGI-KDCALL                               
133501     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
133601     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
133701     MOVE '2392'             TO MSGI-IDTRANS                              
133801                                                                          
133901     IF EGEN-MID                                                          
134001     OR (MID-IDARTNR-IN NUMERIC                                           
134101     AND MID-IDARTNR-IN > ZERO)                                           
134201        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
134301        MOVE WS-IDDC-CDC     TO MSGI-IDDC-KEY                             
134401     END-IF                                                               
134501                                                                          
134601     MOVE SPACE              TO MSGI-SPAR-AREA                            
134701     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
134801     MOVE MSGI-IDARTNR       TO WS-IDARTNR                                
134901                                                                          
135001     MOVE WS-IDDC-CDC        TO IDDC-WS                                   
135101                                WS-IDDC                                   
135201                                W-IDDC                                    
135301                                W-IDDC-301                                
135401                                W-IDDC-MIN                                
135501                                W-IDDC-MAX                                
135601                                W-IDDC-B6                                 
135701                                                                          
135801*  ----  GET COUNTRY INFORMATION FOR REFILLING DC                         
135901     PERFORM IMS-GU-WDB601                                                
136001     MOVE DCS-IDLANDX2       TO W-IDLAND                                  
136101*                                                                         
136201     IF WS-IDARTNR NUMERIC                                                
136301        MOVE WS-IDARTNR-NUM                                               
136401                             TO W-IDARTNR                                 
136501                                W-IDARTNR-SAVED                           
136601     ELSE                                                                 
136701        MOVE NEJ             TO NYCKLAR-SW                                
136801     END-IF                                                               
136901                                                                          
137001     MOVE MSGI-SPAR-AREA(1:55)  TO WS-A2                                  
137101     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
137201                                                                          
137301     IF EGEN-MID                                                          
137401                                                                          
137501       IF MSGI-SPAR-AREA(1:4) = '2392'                                    
137601         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2392                         
137701         IF WS-MSGI-IDARTNR-ENTER NUMERIC                                 
137801            CONTINUE                                                      
137901         ELSE                                                             
138001            MOVE ALL ZERO    TO WS-MSGI-IDARTNR-ENTER                     
138101         END-IF                                                           
138201         IF WS-MSGI-IDARTNR-PF7   NUMERIC                                 
138301            CONTINUE                                                      
138401         ELSE                                                             
138501            MOVE ALL ZERO    TO WS-MSGI-IDARTNR-PF7                       
138601         END-IF                                                           
138602         MOVE WS-MSGI-IDDC-REF TO REF-WS-IDDC                             
138603         IF REF-GOOD-DC                                                   
138604           MOVE WS-MSGI-IDDC-REF TO WS-IDDC-REF-KEY                       
138605                                    W-IDDC-REF-MIN                        
138606                                    W-IDDC-REF-MAX                        
138607         ELSE                                                             
138608           MOVE SPACE            TO WS-IDDC-REF-KEY                       
138609                                    W-IDDC-REF-MIN                        
138610           MOVE HIGH-VALUE       TO W-IDDC-REF-MAX                        
138620         END-IF                                                           
138701       END-IF                                                             
138801       MOVE JA TO NYCKLAR-SW                                              
138901                                                                          
139001*---   CONTROL ON PART NUMBER                                             
139101                                                                          
139201       IF WS-IDARTNR NUMERIC                                              
139301         MOVE WS-IDARTNR-NUM                                              
139401                             TO W-IDARTNR-MIN                             
139501                                W-IDARTNR-301                             
139601                                W-IDARTNR                                 
139701       ELSE                                                               
139801         MOVE NEJ            TO NYCKLAR-SW                                
139901       END-IF                                                             
140001                                                                          
140101       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
140201                                                                          
140301       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
140401       INSPECT MOD-IDARTNR-UT REPLACING LEADING '+'  BY SPACE             
140501                                                                          
140601       PERFORM S7-CHECK-REFILL-PART-K6                                    
140701                                                                          
140801*      -- CONTROL ON TYP                                                  
140901*      -- TYP USED FOR SEARCH OF REFILL ORDER PROPOSALS                   
141001                                                                          
141101       IF MID-IDTYPE-IN = ALL '+'                                         
141201         INSPECT MID-IDTYPE-UT REPLACING LEADING '+' BY SPACE             
141301         IF MID-IDTYPE-UT = 'AIRCR'                                       
141401           MOVE 'C'          TO WS-IDTYPE                                 
141501         ELSE                                                             
141601           MOVE MID-IDTYPE-UT                                             
141701                             TO WS-IDTYPE                                 
141801         END-IF                                                           
141901       ELSE                                                               
142001         MOVE MID-IDTYPE-IN  TO WS-IDTYPE                                 
142101                                MID-IDREFTYP-UT                           
142201       END-IF                                                             
142301                                                                          
142401       IF WS-IDTYPE = 'A'                                                 
142501       OR WS-IDTYPE = 'B'                                                 
142601       OR WS-IDTYPE = 'C'                                                 
142701         IF WS-IDTYPE = 'A'                                               
142801           MOVE 'AIR'        TO MOD-IDTYPE-UT                             
142901         END-IF                                                           
143001         IF WS-IDTYPE = 'C'                                               
143101           MOVE 'AIRCR'      TO MOD-IDTYPE-UT                             
143201         END-IF                                                           
143301         IF WS-IDTYPE = 'B'                                               
143401           MOVE 'BOAT'       TO MOD-IDTYPE-UT                             
143501         END-IF                                                           
143601       ELSE                                                               
143701         IF MID-IDTYPE-IN NOT = ALL '+'                                   
143801           MOVE NEJ          TO NYCKLAR-SW                                
143901           MOVE MED-1        TO MOD-TEMFSINF                              
144001         END-IF                                                           
144101       END-IF                                                             
144201       INSPECT MOD-IDTYPE-UT REPLACING LEADING '+'  BY SPACE              
144301                                                                          
144401*      -- KONTROLL AV IDPERSON-BUY                                        
144501                                                                          
144601       IF MID-IDPERSON-BUY-IN = ALL '+'                                   
144701         INSPECT MID-IDPERSON-BUY-UT                                      
144801                                   REPLACING LEADING '+' BY SPACE         
144901         MOVE MID-IDPERSON-BUY-UT                                         
145001                             TO WS-IDPERSON-BUY                           
145101       ELSE                                                               
145201           MOVE MID-IDPERSON-BUY-IN                                       
145301                             TO WS-IDPERSON-BUY                           
145401       END-IF                                                             
145501       INSPECT WS-IDPERSON-BUY REPLACING LEADING SPACE BY ZERO            
145601       IF WS-IDPERSON-BUY-NUM NUMERIC                                     
145701         MOVE WS-IDPERSON-BUY-NUM                                         
145801                               TO W-IDPERSON-BUY                          
145901                                  WS-IDPERSON-BUY-RED                     
146001                                                                          
146101         MOVE WS-IDPERSON-BUY-RED                                         
146201                               TO MOD-IDPERSON-BUY-UT                     
146301         INSPECT MOD-IDPERSON-BUY-UT REPLACING LEADING '+'                
146401                                     BY SPACE                             
146501       ELSE                                                               
146601         MOVE NEJ TO NYCKLAR-SW                                           
146701       END-IF                                                             
146702                                                                          
146810                                                                          
146901*      -- CONTROL ON STATUS                                               
147001                                                                          
147101       IF MID-IDSTATUS-IN = ALL '+'                                       
147201         INSPECT MID-IDSTATUS-UT REPLACING LEADING '+' BY SPACE           
147301         MOVE MID-IDSTATUS-UT  TO WS-STATUS                               
147401       ELSE                                                               
147501         MOVE MID-IDSTATUS-IN  TO WS-STATUS                               
147601       END-IF                                                             
147701                                                                          
147801       IF WS-STATUS = 'R'                                                 
147901       OR WS-STATUS = 'N'                                                 
148001         IF WS-STATUS = 'R'                                               
148101           MOVE 'REVIEWED'   TO MOD-IDSTATUS-UT                           
148201         END-IF                                                           
148301         IF WS-STATUS = 'N'                                               
148401           MOVE 'NOT REVIEWED'                                            
148501                             TO MOD-IDSTATUS-UT                           
148601         END-IF                                                           
148701       ELSE                                                               
148801         IF MID-IDSTATUS-IN NOT = ALL '+'                                 
148901           MOVE NEJ TO NYCKLAR-SW                                         
149001           MOVE MED-2        TO MOD-TEMFSINF                              
149101         END-IF                                                           
149201       END-IF                                                             
149301       INSPECT MOD-IDSTATUS-UT REPLACING LEADING '+'  BY SPACE            
149401                                                                          
149501*      -- CONTROL ON IDREFTYP                                             
149601*      -- IDREFTYP USED TO SPECIFY THE ORDERTYP                           
149701                                                                          
149801       IF MID-IDREFTYP-IN = ALL '+'                                       
149901         IF MID-IDREFTYP-UT (1:1) = 'B'                                   
150001         OR MID-IDREFTYP-UT (1:1) = 'A'                                   
150101         OR MID-IDREFTYP-UT       = 'AIRCR'                               
150201           IF MID-IDREFTYP-UT = 'AIRCR'                                   
150301             MOVE 'C'        TO WS-IDREFTYP                               
150401           ELSE                                                           
150501             MOVE MID-IDREFTYP-UT                                         
150601                             TO WS-IDREFTYP                               
150701           END-IF                                                         
150801         ELSE                                                             
150901           MOVE WS-IDTYPE    TO WS-IDREFTYP                               
151001         END-IF                                                           
151101       ELSE                                                               
151201         MOVE MID-IDREFTYP-IN                                             
151301                             TO WS-IDREFTYP                               
151401       END-IF                                                             
151501       IF WS-IDREFTYP = 'B'                                               
151601       OR WS-IDREFTYP = 'A'                                               
151701       OR WS-IDREFTYP = 'C'                                               
151801         IF WS-IDREFTYP = 'B'                                             
151901           MOVE 'BOAT'       TO MOD-IDREFTYP-UT                           
152001         END-IF                                                           
152101         IF WS-IDREFTYP = 'A'                                             
152201           MOVE 'AIR'        TO MOD-IDREFTYP-UT                           
152301         END-IF                                                           
152401         IF WS-IDREFTYP = 'C'                                             
152501           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
152601         END-IF                                                           
152701       ELSE                                                               
152801         IF MID-IDREFTYP-IN NOT = ALL '+'                                 
152901           MOVE NEJ          TO NYCKLAR-SW                                
153001           MOVE MED-7        TO MOD-TEMFSINF                              
153101         END-IF                                                           
153201       END-IF                                                             
153301       INSPECT MOD-IDREFTYP-UT REPLACING LEADING '+'  BY SPACE            
153401     ELSE                                                                 
153501                                                                          
153601       IF  MFS-IDTRANS = '2391'                                           
153701                                                                          
153801         MOVE +1             TO IX                                        
153901         PERFORM UNTIL IX > +13                                           
154001         OR MID-SELECT-2391 (IX) NOT = '+'                                
154101           ADD +1            TO IX                                        
154201         END-PERFORM                                                      
154301                                                                          
154401         MOVE ZERO           TO WS-IDPERSON-BUY                           
154501         MOVE SPACE          TO WS-IDTYPE                                 
154601                                WS-IDREFTYP                               
154701                                                                          
154801         IF IX > +13                                                      
154901           CONTINUE                                                       
155001         ELSE                                                             
155101           INSPECT MID-IDPERSON-2391 (IX) REPLACING                       
155201                                     LEADING SPACE BY ZERO                
155301             IF MID-IDTYPE-2391-IN = '+'                                  
155401               IF MID-IDTYPE-2391-UT = 'AIR'                              
155501                 MOVE 'A'    TO WS-IDTYPE                                 
155601                                WS-IDREFTYP                               
155701               END-IF                                                     
155801               IF MID-IDTYPE-2391-UT = 'AIRCR'                            
155901                 MOVE 'C'    TO WS-IDTYPE                                 
156001                                WS-IDREFTYP                               
156101               END-IF                                                     
156201               IF MID-IDTYPE-2391-UT = 'BOAT'                             
156301                 MOVE 'B'    TO WS-IDTYPE                                 
156401                                WS-IDREFTYP                               
156501               END-IF                                                     
156601             ELSE                                                         
156701               MOVE MID-IDTYPE-2391-IN                                    
156801                             TO WS-IDTYPE                                 
156901                                WS-IDREFTYP                               
157001             END-IF                                                       
157101             MOVE MID-IDPERSON-2391 (IX)                                  
157201                             TO WS-IDPERSON-BUY                           
157301         END-IF                                                           
157302         MOVE MID-IDDC-REF-2391-UT TO REF-WS-IDDC                         
157303         IF MID-IDDC-REF-2391-UT   NOT = SPACE                            
157305         AND REF-GOOD-DC                                                  
157306           IF REF-GOOD-DC                                                 
157307             MOVE MID-IDDC-REF-2391-UT   TO WS-IDDC-REF-KEY               
157308                                            WS-MSGI-IDDC-REF              
157309           ELSE                                                           
157310             MOVE NEJ           TO NYCKLAR-SW                             
157311             MOVE MED-19        TO MOD-TEMFSINF                           
157314           END-IF                                                         
157320         ELSE                                                             
157330           MOVE SPACE             TO WS-IDDC-REF-KEY                      
157331                                     WS-MSGI-IDDC-REF                     
157340         END-IF                                                           
157401                                                                          
157501         MOVE '7'            TO MFS-IDPFK                                 
157601         MOVE SPACE          TO MFS-KDTRTYP                               
157701                                                                          
157801         MOVE 'N'            TO WS-STATUS                                 
157901         INSPECT WS-IDPERSON-BUY REPLACING                                
158001                                     LEADING SPACE BY ZERO                
158101         IF WS-IDPERSON-BUY-NUM NUMERIC                                   
158201           MOVE WS-IDPERSON-BUY-NUM                                       
158301                             TO W-IDPERSON-BUY                            
158401                                WS-IDPERSON-BUY-RED                       
158501                                                                          
158601           MOVE WS-IDPERSON-BUY-RED                                       
158701                             TO MOD-IDPERSON-BUY-UT                       
158801           INSPECT MOD-IDPERSON-BUY-UT REPLACING                          
158901                                     LEADING '+'      BY SPACE            
159001         ELSE                                                             
159101           MOVE NEJ TO NYCKLAR-SW                                         
159201         END-IF                                                           
159301                                                                          
159401         MOVE IDDC-WS        TO W-IDDC                                    
159501                                WS-IDDC                                   
159601                                                                          
159701         IF WS-IDTYPE = 'A'                                               
159801         OR WS-IDTYPE = 'B'                                               
159901         OR WS-IDTYPE = 'C'                                               
160001           IF WS-IDTYPE = 'A'                                             
160101             MOVE 'AIR'      TO MOD-IDTYPE-UT                             
160201                                MOD-IDREFTYP-UT                           
160301           END-IF                                                         
160401           IF WS-IDTYPE = 'C'                                             
160501             MOVE 'AIRCR'    TO MOD-IDTYPE-UT                             
160601                                MOD-IDREFTYP-UT                           
160701           END-IF                                                         
160801           IF WS-IDTYPE = 'B'                                             
160901             MOVE 'BOAT'     TO MOD-IDTYPE-UT                             
161001                                MOD-IDREFTYP-UT                           
161101           END-IF                                                         
161201         ELSE                                                             
161301           MOVE NEJ TO NYCKLAR-SW                                         
161401         END-IF                                                           
161501         INSPECT MOD-IDTYPE-UT REPLACING                                  
161601                                   LEADING '+' BY SPACE                   
161701                                                                          
161801         MOVE 'N'            TO WS-STATUS                                 
161901         MOVE 'NOT REVIEWED'                                              
162001                             TO MOD-IDSTATUS-UT                           
162101                                                                          
162201       ELSE                                                               
162301*      -- CONTROL ON IDARTNR                                              
162401                                                                          
162501         IF WS-IDARTNR NUMERIC                                            
162601           MOVE WS-IDARTNR-NUM                                            
162701                             TO W-IDARTNR-MIN                             
162801                                W-IDARTNR-MAX                             
162901                                W-IDARTNR-301                             
163001                                W-IDARTNR                                 
163101                                MID-IDARTNR-IN                            
163201         ELSE                                                             
163301           MOVE NEJ          TO NYCKLAR-SW                                
163401         END-IF                                                           
163501                                                                          
163601         MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                            
163701                                                                          
163801         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
163901         INSPECT MOD-IDARTNR-UT REPLACING LEADING '+' BY SPACE            
164001                                                                          
164101         MOVE IDDC-WS          TO W-IDDC                                  
164201         MOVE IDDC-WS          TO WS-IDDC                                 
164301                                                                          
164401         IF MSGI-SPAR-AREA(1:4) = '2392'                                  
164501           MOVE MSGI-SPAR-AREA                                            
164601                               TO WS-MSGI-AREA-2392                       
164701           IF WS-MSGI-IDARTNR-ENTER NUMERIC                               
164801              CONTINUE                                                    
164901           ELSE                                                           
165001              MOVE ALL ZERO    TO WS-MSGI-IDARTNR-ENTER                   
165101           END-IF                                                         
165201           IF WS-MSGI-IDARTNR-PF7   NUMERIC                               
165301              CONTINUE                                                    
165401           ELSE                                                           
165501              MOVE ALL ZERO    TO WS-MSGI-IDARTNR-PF7                     
165601           END-IF                                                         
165701*                                                                         
165801           IF WS-MSGI-IDTYPE    = 'A'                                     
165901           OR WS-MSGI-IDTYPE    = 'B'                                     
166001           OR WS-MSGI-IDTYPE    = 'C'                                     
166101             IF WS-MSGI-IDTYPE  = 'A'                                     
166201               MOVE 'AIR'      TO MOD-IDTYPE-UT                           
166301                                  MOD-IDREFTYP-UT                         
166401             END-IF                                                       
166501             IF WS-MSGI-IDTYPE  = 'C'                                     
166601               MOVE 'AIRCR'    TO MOD-IDTYPE-UT                           
166701                                  MOD-IDREFTYP-UT                         
166801             END-IF                                                       
166901             IF WS-MSGI-IDTYPE  = 'B'                                     
167001               MOVE 'BOAT'     TO MOD-IDTYPE-UT                           
167101                                  MOD-IDREFTYP-UT                         
167201             END-IF                                                       
167301             MOVE WS-MSGI-IDTYPE                                          
167401                               TO MID-IDTYPE-IN                           
167501                                  MID-IDREFTYP-IN                         
167601                                  WS-IDTYPE                               
167701                                  WS-IDREFTYP                             
167801           ELSE                                                           
167901             MOVE '+'          TO MID-IDTYPE-IN                           
168001                                  MID-IDREFTYP-IN                         
168101           END-IF                                                         
168201         ELSE                                                             
168301           MOVE '+'            TO MID-IDTYPE-IN                           
168401                                  MID-IDREFTYP-IN                         
168501         END-IF                                                           
168601                                                                          
168701         MOVE 'N'              TO WS-STATUS                               
168801         MOVE 'NOT REVIEWED'                                              
168901                               TO MOD-IDSTATUS-UT                         
169001         PERFORM S7-CHECK-REFILL-PART-K6                                  
169002*                                                                         
169003         MOVE MSGI-IDDC-REF TO REF-WS-IDDC                                
169004         IF REF-GOOD-DC                                                   
169005           MOVE    MSGI-IDDC-REF TO WS-IDDC-REF-KEY                       
169006                                    W-IDDC-REF-MIN                        
169007                                    W-IDDC-REF-MAX                        
169009                                    MID-IDDC-REF-UT                       
169010         ELSE                                                             
169011           MOVE SPACE            TO WS-IDDC-REF-KEY                       
169012                                    W-IDDC-REF-MIN                        
169014                                    MID-IDDC-REF-UT                       
169015           MOVE HIGH-VALUE       TO W-IDDC-REF-MAX                        
169016         END-IF                                                           
169101       END-IF                                                             
169201     END-IF                                                               
169301                                                                          
169401     IF NYCKLAR-FEL                                                       
169501       IF NOT-REFILL-PART                                                 
169601          MOVE ERR-NOT-REFILL-PART                                        
169701                                 TO MED-IDMFSFEL                          
169801       ELSE                                                               
169901          MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                          
170001       END-IF                                                             
170101       IF DISTRICT-NOT-FOUND                                              
170201          MOVE MED-15            TO MOD-TEMFSFEL                          
170301       ELSE                                                               
170401         IF REFILL-ALLOWED-NEJ                                            
170501            MOVE MED-16          TO MOD-TEMFSFEL                          
170601         ELSE                                                             
170701            CALL WMEDKONV     USING MED-WMEDAREA                          
170801            MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                          
170901         END-IF                                                           
171001       END-IF                                                             
171101       PERFORM MFS-RENSA-FAELT-IN                                         
171201       PERFORM MFS-RENSA-FAELT-UT                                         
171301     ELSE                                                                 
171401       IF WS-IDPERSON-BUY NUMERIC                                         
171501         MOVE WS-IDPERSON-BUY                                             
171601                                 TO W-IDPERSON-BUY                        
171701         MOVE WS-IDPERSON-BUY-NUM                                         
171801                                 TO W-IDPERSON-BUY-MIN                    
171901                                    W-IDPERSON-BUY-MAX                    
172001       ELSE                                                               
172101         MOVE ZERO               TO WS-IDPERSON-BUY                       
172201                                    W-IDPERSON-BUY                        
172301       END-IF                                                             
172401       MOVE WS-IDREFTYP          TO W-KDREFTYP-MIN                        
172501                                    W-KDREFTYP-MAX                        
172601       MOVE WS-IDTYPE            TO WS-MSGI-IDTYPE                        
172602                                                                          
172603       IF WS-IDDC-REF-KEY = SPACE                                         
172604         MOVE SPACE              TO WS-MSGI-IDDC-REF                      
172605       ELSE                                                               
172606         MOVE WS-IDDC-REF-KEY    TO W-IDDC-REF-MIN                        
172607                                    W-IDDC-REF-MAX                        
172608                                    WS-MSGI-IDDC-REF                      
172609       END-IF                                                             
172701     END-IF                                                               
172801     .                                                                    
172901     EJECT                                                                
173001                                                                          
173101 C-FOEREG-SIDA SECTION.                                                   
173201     MOVE NEJ                TO SW-TRAEFF                                 
173301                                                                          
173401     PERFORM CA-BLAEDDRA-BAK                                              
173501                                                                          
173601     MOVE WS-ANTAL-POSTER    TO WS-ANTAL-TF                               
173701     IF SW-TRAEFF-JA                                                      
173801       CONTINUE                                                           
173901     ELSE                                                                 
174001       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
174101       CALL WMEDKONV USING MED-WMEDAREA                                   
174201       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
174301       IF EGEN-MID                                                        
174401         MOVE ZERO           TO W-IDARTNR                                 
174501       END-IF                                                             
174601       PERFORM MFS-RENSA-FAELT-IN                                         
174701       PERFORM MFS-RENSA-FAELT-UT                                         
174801     END-IF                                                               
174901     .                                                                    
175001     EJECT                                                                
175101                                                                          
175201 CA-BLAEDDRA-BAK SECTION.                                                 
175301                                                                          
175401     MOVE NEJ                    TO SW-TRAEFF                             
175501****   FOR DISPLAY OF FIRST PAGE ARTICLE        *******                   
175601****   (IF ANY)                                 *******                   
175701     MOVE WS-MSGI-IDARTNR-PF7                                             
175801                                 TO W-IDARTNR                             
175901                                    W-IDARTNR-301                         
176001                                    W-IDARTNR-MIN                         
176101                                                                          
176201     PERFORM IMS-GU-WDE301-MIN-MAX                                        
176301                                                                          
176401     PERFORM UNTIL SEGMENT-SAKNAS                                         
176501     OR (REF-IDARTNR              = WS-MSGI-IDARTNR-PF7                   
176601     AND  REF-IDPERSON-BUY        = WS-IDPERSON-BUY-NUM                   
176701     AND  REF-KDREFTYP            = WS-IDTYPE                             
176801     AND  ((WS-MSGI-ORDER-PF7     = 'N'                                   
176901       AND REF-KDREFORS           = 'P')                                  
177001       OR (WS-MSGI-ORDER-PF7      = 'R'                                   
177101       AND REF-KDREFORS           = 'O')))                                
177201                                                                          
177301          PERFORM IMS-GN-WDE301-MIN-MAX                                   
177401                                                                          
177501     END-PERFORM                                                          
177601                                                                          
177701     IF SEGMENT-FINNS                                                     
177801*       --- CHECK IDLEVNR-SECURITY                                        
177901        MOVE REF-IDARTNR         TO W-IDARTNR                             
178101        PERFORM S1-SECURITY-CHECK-PARTNO                                  
178201     END-IF                                                               
178301                                                                          
178401     IF SEGMENT-FINNS                                                     
178501                                                                          
178601       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
178701                                                                          
178801     ELSE                                                                 
178901       MOVE ZERO                 TO W-IDARTNR                             
179001                                    W-IDARTNR-301                         
179101                                    W-IDARTNR-MIN                         
179201                                                                          
179301       PERFORM IMS-GU-WDE301-MIN-MAX                                      
179401                                                                          
179501       PERFORM UNTIL SEGMENT-SAKNAS                                       
179601                                                                          
179701       OR (REF-IDPERSON-BUY       = WS-IDPERSON-BUY-NUM                   
179801       AND (REF-KDREFTYP          = WS-IDTYPE                             
179901       AND ((WS-STATUS            = 'N'                                   
180001       AND REF-KDREFORS           = 'P')                                  
180101       OR (WS-STATUS              = 'R'                                   
180201       AND REF-KDREFORS           = 'O'))))                               
180301                                                                          
180401           ADD +1                TO WS-ANTAL-POSTER                       
180501                                                                          
180601           PERFORM IMS-GN-WDE301-MIN-MAX                                  
180701                                                                          
180801       END-PERFORM                                                        
180901                                                                          
181001       IF SEGMENT-FINNS                                                   
181101*       --- CHECK IDLEVNR-SECURITY                                        
181201          MOVE REF-IDARTNR       TO W-IDARTNR                             
181401          PERFORM S1-SECURITY-CHECK-PARTNO                                
181501       END-IF                                                             
181601                                                                          
181701       IF SEGMENT-FINNS                                                   
181801                                                                          
181901         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
182001                                                                          
182101       END-IF                                                             
182201     END-IF                                                               
182301                                                                          
182401     PERFORM UNTIL SEGMENT-SAKNAS                                         
182501     OR WS-SPARA-IDARTNR      NOT = REF-IDARTNR                           
182601********      READ ALL REFILL ITEMS FOR THE CURRENT                       
182701********      ARTNR FOR CDC WITH PROPER KDREFTYP                          
182801********                                                                  
182901       IF REF-KDREFTYP            = WS-IDTYPE                             
183001         PERFORM M-UPD-MOD-FAELT                                          
183101       END-IF                                                             
183201                                                                          
183301       PERFORM IMS-GN-WDE301-MIN-MAX                                      
183401     END-PERFORM                                                          
183501     .                                                                    
183601     EJECT                                                                
183701                                                                          
183801 D-NAESTA-SIDA SECTION.                                                   
183901     MOVE WS-IDPERSON-BUY    TO W-IDPERSON-BUY-MIN                        
184001     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
184101     MOVE WS-IDARTNR         TO W-IDARTNR-MIN                             
184201     MOVE ZERO               TO W-IDDISTR-MIN                             
184301     MOVE NEJ                TO SW-TRAEFF                                 
184401                                                                          
184501     PERFORM DA-BLAEDDRA-FRAM                                             
184601                                                                          
184701     IF SW-TRAEFF-JA                                                      
184801       CONTINUE                                                           
184901     ELSE                                                                 
185001       MOVE URVAL-SAKNAS     TO MED-IDMFSFEL                              
185101       CALL WMEDKONV USING MED-WMEDAREA                                   
185201       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
185301       MOVE ZERO             TO W-IDARTNR                                 
185401       PERFORM MFS-RENSA-FAELT-IN                                         
185501       PERFORM MFS-RENSA-FAELT-UT                                         
185601     END-IF                                                               
185701     .                                                                    
185801     EJECT                                                                
185901                                                                          
186001 DA-BLAEDDRA-FRAM SECTION.                                                
186101                                                                          
186201     PERFORM IMS-GU-WDE301-PF8                                            
186301                                                                          
186401     PERFORM UNTIL SEGMENT-SAKNAS                                         
186501     OR  (REF-IDPERSON-BUY    = WS-IDPERSON-BUY-NUM                       
186601     AND  REF-IDARTNR         > W-IDARTNR                                 
186701     AND (REF-KDREFTYP        = WS-IDTYPE                                 
186801     AND ((WS-STATUS          = 'N'                                       
186901     AND       REF-KDREFORS   = 'P')                                      
187001     OR      (WS-STATUS       = 'R'                                       
187101     AND  REF-KDREFORS        = 'O'))))                                   
187201                                                                          
187301         PERFORM IMS-GN-WDE301-PF8                                        
187401     END-PERFORM                                                          
187501                                                                          
187601     IF SEGMENT-FINNS                                                     
187701*       --- CHECK IDLEVNR-SECURITY                                        
187801        MOVE REF-IDARTNR     TO W-IDARTNR                                 
187901        PERFORM S1-SECURITY-CHECK-PARTNO                                  
188001     END-IF                                                               
188101                                                                          
188201     IF SEGMENT-FINNS                                                     
188301       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
188401     ELSE                                                                 
188501       MOVE ZERO             TO W-IDARTNR                                 
188601     END-IF                                                               
188701                                                                          
188801     MOVE WS-MSGI-IDARTNR-ENTER                                           
188901                             TO WS-MSGI-IDARTNR-PF7                       
189001     MOVE WS-MSGI-ORDER-ENTER                                             
189101                             TO WS-MSGI-ORDER-PF7                         
189201                                                                          
189301     PERFORM UNTIL SEGMENT-SAKNAS                                         
189401     OR WS-SPARA-IDARTNR NOT  = REF-IDARTNR                               
189501**********      READ ALL REFILL ITEMS FOR THE CURRENT                     
189601**********      ARTNR FOR CDC WITH PROPER KDREFTYP                        
189701**********                                                                
189801       IF REF-KDREFTYP        = WS-IDTYPE                                 
189901         PERFORM M-UPD-MOD-FAELT                                          
190001       END-IF                                                             
190101                                                                          
190201       PERFORM IMS-GN-WDE301-PF8                                          
190301     END-PERFORM                                                          
190401     .                                                                    
190501     EJECT                                                                
190601                                                                          
190701 E-SAMMA-SIDA SECTION.                                                    
190801                                                                          
190901     IF MID-IDARTNR-IN        = ALL '+'                                   
191001     AND MID-IDTYPE-IN        = ALL '+'                                   
191101     AND MID-IDPERSON-BUY-IN  = ALL '+'                                   
191201     AND MID-IDSTATUS-IN      = ALL '+'                                   
191301     AND MID-IDREFTYP-IN      = ALL '+'                                   
191401                                                                          
191501       PERFORM EB-SIMULERA                                                
191601       IF MID-INPUT      NOT  = ALL '+'                                   
191701         PERFORM I-KOLLA-INPUT                                            
191901         MOVE WS-PURCHQTY    TO WS-PURCHQTY-SIM                           
192001       END-IF                                                             
192101     ELSE                                                                 
192201                                                                          
192301       IF (MID-IDARTNR-IN NOT = ALL '+'                                   
192401       AND MID-IDTYPE-IN  NOT = ALL '+')                                  
192501       OR MID-IDREFTYP-IN NOT = ALL '+'                                   
192601                                                                          
192701         PERFORM EC-SOEK-NY-ARTIKEL-TYP                                   
192801                                                                          
192901         IF SW-TRAEFF-JA                                                  
193001           CONTINUE                                                       
193101         ELSE                                                             
193201           PERFORM MFS-RENSA-FAELT-IN                                     
193301           PERFORM MFS-RENSA-FAELT-UT                                     
193401           MOVE URVAL-SAKNAS TO MED-IDMFSFEL                              
193501           CALL WMEDKONV  USING MED-WMEDAREA                              
193601           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
193701         END-IF                                                           
193801                                                                          
193901       ELSE                                                               
194001         IF  MID-IDARTNR-IN NOT = ALL '+'                                 
194101         AND MID-IDTYPE-IN      = ALL '+'                                 
194201         AND MID-IDREFTYP-IN    = ALL '+'                                 
194301                                                                          
194401            PERFORM ED-SOEK-NY-ARTIKEL                                    
194501                                                                          
194601            IF SW-TRAEFF-JA                                               
194701              CONTINUE                                                    
194801            ELSE                                                          
194901              PERFORM MFS-RENSA-FAELT-IN                                  
195001              PERFORM MFS-RENSA-FAELT-UT                                  
195101              MOVE URVAL-SAKNAS TO MED-IDMFSFEL                           
195201              CALL WMEDKONV  USING MED-WMEDAREA                           
195301              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
195401            END-IF                                                        
195501                                                                          
195601         ELSE                                                             
195701           IF MID-IDPERSON-BUY-IN NOT = ALL '+'                           
195801           OR MID-IDTYPE-IN       NOT = ALL '+'                           
195901           OR MID-IDSTATUS-IN     NOT = ALL '+'                           
196001                                                                          
196101             PERFORM EA-NY-BUY-TYPE-LEV-STAT                              
196201                                                                          
196301             IF SW-TRAEFF-JA                                              
196401               CONTINUE                                                   
196501             ELSE                                                         
196601               PERFORM MFS-RENSA-FAELT-IN                                 
196701               PERFORM MFS-RENSA-FAELT-UT                                 
196801               MOVE URVAL-SAKNAS TO MED-IDMFSFEL                          
196901               CALL WMEDKONV  USING MED-WMEDAREA                          
197001               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
197101             END-IF                                                       
197201           END-IF                                                         
197301         END-IF                                                           
197401       END-IF                                                             
197501     END-IF                                                               
197601     .                                                                    
197701     EJECT                                                                
197801                                                                          
197901 EA-NY-BUY-TYPE-LEV-STAT SECTION.                                         
198001                                                                          
198101     MOVE NEJ                TO SW-TRAEFF                                 
198201     MOVE ZERO               TO W-IDARTNR                                 
198301     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UT                            
198401     PERFORM IMS-GU-WDE301-MIN-MAX                                        
198501                                                                          
198601     PERFORM UNTIL SEGMENT-SAKNAS                                         
198701     OR  (REF-KDREFTYP        = WS-IDTYPE                                 
198801     AND ((WS-STATUS          = 'N'                                       
198901     AND  REF-KDREFORS        = 'P')                                      
199001     OR  (WS-STATUS           = 'R'                                       
199101     AND  REF-KDREFORS        = 'O')))                                    
199201          PERFORM IMS-GN-WDE301-MIN-MAX                                   
199301     END-PERFORM                                                          
199401                                                                          
199501     IF SEGMENT-FINNS                                                     
199601*       --- CHECK IDLEVNR-SECURITY                                        
199701        MOVE REF-IDARTNR     TO W-IDARTNR                                 
199801        PERFORM S1-SECURITY-CHECK-PARTNO                                  
199901     END-IF                                                               
200001                                                                          
200101     IF SEGMENT-FINNS                                                     
200201       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
200301     END-IF                                                               
200401                                                                          
200501     PERFORM UNTIL SEGMENT-SAKNAS                                         
200601     OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                                
200701       IF REF-KDREFTYP = WS-IDTYPE                                        
200801         PERFORM M-UPD-MOD-FAELT                                          
200901       END-IF                                                             
201001                                                                          
201101       PERFORM IMS-GN-WDE301-MIN-MAX                                      
201201     END-PERFORM                                                          
201301     .                                                                    
201401     EJECT                                                                
201501                                                                          
201601 EB-SIMULERA SECTION.                                                     
201701                                                                          
201801     MOVE NEJ                TO SW-TRAEFF                                 
201901     MOVE WS-MSGI-IDARTNR-ENTER                                           
202001                             TO W-IDARTNR                                 
202101                                W-IDARTNR-301                             
202201                                W-IDARTNR-MIN                             
202301     PERFORM IMS-GU-WDE301-MIN-MAX                                        
202401                                                                          
202501     PERFORM UNTIL SEGMENT-SAKNAS                                         
202601     OR (REF-IDARTNR          = W-IDARTNR-MIN                             
202701     AND REF-KDREFTYP         = WS-IDREFTYP)                              
202801         PERFORM IMS-GN-WDE301-MIN-MAX                                    
202901     END-PERFORM                                                          
203001                                                                          
203101     IF SEGMENT-FINNS                                                     
203201*       --- CHECK IDLEVNR-SECURITY                                        
203301        MOVE REF-IDARTNR     TO W-IDARTNR                                 
203401        PERFORM S1-SECURITY-CHECK-PARTNO                                  
203501     END-IF                                                               
203601                                                                          
203701     IF SEGMENT-FINNS                                                     
203801       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
203901     END-IF                                                               
204001                                                                          
204101     PERFORM UNTIL SEGMENT-SAKNAS                                         
204201     OR WS-SPARA-IDARTNR  NOT = REF-IDARTNR                               
204301       IF REF-KDREFTYP        = WS-IDREFTYP                               
204401                                                                          
204501         MOVE REF-KDREFTXT   TO WS-REF-KDREFTXT                           
204701         IF REF-KDREFORS      = 'P'                                       
204801            MOVE 'PROPOSAL NOT REVIEWED'                                  
204901                             TO MOD-ORDERSTATUS                           
205001         ELSE                                                             
205101            MOVE 'REVIEWED'  TO MOD-ORDERSTATUS                           
205201         END-IF                                                           
205301                                                                          
205401         IF REF-KDREFTYP      = 'B'                                       
205501           MOVE 'BOAT '      TO MOD-IDREFTYP-UT                           
205601         END-IF                                                           
205701         IF REF-KDREFTYP      = 'A'                                       
205801           MOVE 'AIR  '      TO MOD-IDREFTYP-UT                           
205901         END-IF                                                           
206001         IF REF-KDREFTYP      = 'C'                                       
206101           MOVE 'AIRCR'      TO MOD-IDREFTYP-UT                           
206201         END-IF                                                           
206301                                                                          
206401       END-IF                                                             
206501                                                                          
206601       PERFORM IMS-GN-WDE301-MIN-MAX                                      
206701     END-PERFORM                                                          
206801     .                                                                    
206901     EJECT                                                                
207001                                                                          
207101 EC-SOEK-NY-ARTIKEL-TYP SECTION.                                          
207201                                                                          
207301     MOVE NEJ                TO SW-TRAEFF                                 
207401*    TEXT 'NO PROPOSAL' IS DISPLAYED IF NO MATCH FOUND                    
207501     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
207601     MOVE W-IDARTNR          TO W-IDARTNR-MIN                             
207701     PERFORM IMS-GU-WDE301-MIN-MAX                                        
207801                                                                          
207901     PERFORM UNTIL SEGMENT-SAKNAS                                         
208001     OR (REF-IDARTNR = W-IDARTNR-MIN                                      
208101     AND  REF-KDREFTYP = WS-IDREFTYP)                                     
208201                                                                          
208301       PERFORM IMS-GN-WDE301-MIN-MAX                                      
208401     END-PERFORM                                                          
208501                                                                          
208601     IF SEGMENT-FINNS                                                     
208701*       --- CHECK IDLEVNR-SECURITY                                        
208801        MOVE REF-IDARTNR TO W-IDARTNR                                     
208901        PERFORM S1-SECURITY-CHECK-PARTNO                                  
209001     END-IF                                                               
209101                                                                          
209201     IF SEGMENT-FINNS                                                     
209301       PERFORM K-FYLL-I-NYCKEL-FAELT                                      
209401       MOVE REF-IDDC         TO W-IDDC-301                                
209501       MOVE REF-IDARTNR      TO W-IDARTNR                                 
209601                                W-IDARTNR-301                             
209701                                W-IDARTNR-MIN                             
209801                                W-IDARTNR-MAX                             
209901                                                                          
210001       PERFORM UNTIL SEGMENT-SAKNAS                                       
210101       OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                              
210201                                                                          
210301**********      READ ALL REFILL ITEMS FOR THE CURRENT                     
210401**********      ARTNR FOR CDC WITH PROPER KDREFTYP                        
210501**********                                                                
210601         IF REF-KDREFTYP = WS-IDREFTYP                                    
210701           PERFORM M-UPD-MOD-FAELT                                        
210801         END-IF                                                           
210901                                                                          
211001         PERFORM IMS-GN-WDE301-MIN-MAX                                    
211101       END-PERFORM                                                        
211201     ELSE                                                                 
211301       PERFORM IMS-GU-WDK629                                              
211401       IF SEGMENT-FINNS                                                   
211501         MOVE CREF-IDPERSON-BUY                                           
211601                               TO WS-IDPERSON-BUY-NUM                     
211701                                  WS-IDPERSON-BUY-RED                     
211801         MOVE WS-IDPERSON-BUY-RED                                         
211901                               TO MOD-IDPERSON-BUY-UT                     
212001       END-IF                                                             
212101     END-IF                                                               
212201     .                                                                    
212301     EJECT                                                                
212401                                                                          
212501 ED-SOEK-NY-ARTIKEL SECTION.                                              
212601                                                                          
212701     MOVE NEJ                TO SW-TRAEFF                                 
212801*    TEXT 'NO PROPOSAL' IS DISPLAYED IF NO MATCH FOUND                    
212901     MOVE 'NO PROPOSAL'      TO MOD-ORDERSTATUS                           
213001     MOVE LOW-VALUE          TO W-WDE301KY-MIN-X                          
213101     MOVE IDDC-WS            TO W-IDDC-MIN                                
213201     MOVE HIGH-VALUE         TO W-WDE301KY-MAX-X                          
213301     MOVE IDDC-WS            TO W-IDDC-MAX                                
213401     PERFORM IMS-GU-WDE301-MIN-MAX                                        
213501                                                                          
213601     MOVE 9                  TO WS-KTRL-PRIO                              
213701                                                                          
213801     PERFORM UNTIL SEGMENT-SAKNAS                                         
213901       IF REF-IDARTNR = W-IDARTNR                                         
214001       AND REF-IDDC = W-IDDC                                              
214101         IF (REF-KDREFTYP = 'A' OR 'C')                                   
214201         AND REF-KDREFORS = 'O'                                           
214301         AND WS-KTRL-PRIO > 1                                             
214401*------    HIGHER PRIORITY HANDLED FIRST                                  
214501*------                                                                   
214601           MOVE REF-KDREFTYP TO WS-IDREFTYP                               
214701           MOVE 1            TO WS-KTRL-PRIO                              
214801                                                                          
214901         ELSE                                                             
215001           IF REF-KDREFTYP = 'B'                                          
215101           AND REF-KDREFORS = 'O'                                         
215201           AND WS-KTRL-PRIO > 2                                           
215301*------    HIGHER PRIORITY HANDLED FIRST                                  
215401*------                                                                   
215501             MOVE REF-KDREFTYP TO WS-IDREFTYP                             
215601             MOVE 2          TO WS-KTRL-PRIO                              
215701                                                                          
215801           ELSE                                                           
215901             IF REF-KDREFTYP = 'L'                                        
216001             AND REF-KDREFORS = 'O'                                       
216101             AND WS-KTRL-PRIO > 3                                         
216201*------    HIGHER PRIORITY HANDLED FIRST                                  
216301*------                                                                   
216401               MOVE REF-KDREFTYP TO WS-IDREFTYP                           
216501               MOVE 3        TO WS-KTRL-PRIO                              
216601                                                                          
216701             ELSE                                                         
216801               IF (REF-KDREFTYP = 'A' OR 'C')                             
216901               AND REF-KDREFORS = 'P'                                     
217001               AND WS-KTRL-PRIO > 4                                       
217101*------    HIGHER PRIORITY HANDLED FIRST                                  
217201*------                                                                   
217301                 MOVE REF-KDREFTYP TO WS-IDREFTYP                         
217401                 MOVE 4      TO WS-KTRL-PRIO                              
217501                                                                          
217601               ELSE                                                       
217701                 IF REF-KDREFTYP = 'B'                                    
217801                 AND REF-KDREFORS = 'P'                                   
217901                 AND WS-KTRL-PRIO > 5                                     
218001*------    HIGHER PRIORITY HANDLED FIRST                                  
218101*------                                                                   
218201                   MOVE REF-KDREFTYP TO WS-IDREFTYP                       
218301                   MOVE 5    TO WS-KTRL-PRIO                              
218401                                                                          
218501                 ELSE                                                     
218601                   IF REF-KDREFTYP = 'L'                                  
218701                   AND REF-KDREFORS = 'P'                                 
218801                   AND WS-KTRL-PRIO > 6                                   
218901*------    HIGHER PRIORITY HANDLED FIRST                                  
219001*------                                                                   
219101                     MOVE REF-KDREFTYP TO WS-IDREFTYP                     
219201                     MOVE 6  TO WS-KTRL-PRIO                              
219301                                                                          
219401                   END-IF                                                 
219501                 END-IF                                                   
219601               END-IF                                                     
219701             END-IF                                                       
219801           END-IF                                                         
219901         END-IF                                                           
220001       END-IF                                                             
220101       PERFORM IMS-GN-WDE301-MIN-MAX                                      
220201     END-PERFORM                                                          
220301                                                                          
220401     IF WS-KTRL-PRIO NOT = 9                                              
220501*---    FIND MATCH ON ARTICLE                                             
220601*---                                                                      
220701       PERFORM IMS-GU-WDE301-MIN-MAX                                      
220801       PERFORM UNTIL SEGMENT-SAKNAS                                       
220901       OR (REF-IDARTNR  = W-IDARTNR                                       
221001       AND REF-IDDC     = W-IDDC                                          
221101       AND REF-KDREFTYP = WS-IDREFTYP)                                    
221201*------    READ UNTIL REQUESTED ARTIKEL/REFTYP                            
221301*------                                                                   
221401         PERFORM IMS-GN-WDE301-MIN-MAX                                    
221501       END-PERFORM                                                        
221601                                                                          
221701       IF SEGMENT-FINNS                                                   
221801*         --- CHECK IDLEVNR-SECURITY                                      
221901          MOVE REF-IDARTNR TO W-IDARTNR                                   
222001          PERFORM S1-SECURITY-CHECK-PARTNO                                
222101       END-IF                                                             
222201                                                                          
222301       IF SEGMENT-FINNS                                                   
222401         PERFORM K-FYLL-I-NYCKEL-FAELT                                    
222501         MOVE REF-IDPERSON-BUY                                            
222601                             TO WS-IDPERSON-BUY-NUM                       
222701       ELSE                                                               
222801*------    THIS SHOULD NOT OCCUR        \                                 
222901*------                                                                   
223001         MOVE 'GE'           TO STATUS-WS                                 
223101       END-IF                                                             
223201     ELSE                                                                 
223301       PERFORM IMS-GU-WDK629                                              
223401       IF SEGMENT-FINNS                                                   
223501         MOVE CREF-IDPERSON-BUY                                           
223601                               TO WS-IDPERSON-BUY-NUM                     
223701                                  WS-IDPERSON-BUY-RED                     
223801         MOVE WS-IDPERSON-BUY-RED                                         
223901                               TO MOD-IDPERSON-BUY-UT                     
224001       END-IF                                                             
224101       MOVE 'GE'             TO STATUS-WS                                 
224201     END-IF                                                               
224301                                                                          
224401     PERFORM UNTIL SEGMENT-SAKNAS                                         
224501     OR (WS-SPARA-IDARTNR NOT = REF-IDARTNR                               
224601     AND W-IDDC              = REF-IDDC)                                  
224701                                                                          
224801********** READ ALL REFILL ITEMS FOR CURRENT                              
224901********** ARTNR FOR CDC WITH PROPER KDREFTYP                             
225001**********                                                                
225101       IF REF-KDREFTYP = WS-IDREFTYP                                      
225201         PERFORM M-UPD-MOD-FAELT                                          
225301       END-IF                                                             
225401                                                                          
225501       PERFORM IMS-GN-WDE301-MIN-MAX                                      
225601     END-PERFORM                                                          
225701     .                                                                    
225801     EJECT                                                                
225901                                                                          
226001                                                                          
226101 F-HAEMTA-INFO SECTION.                                                   
226201                                                                          
226301*    ARTICLE INFORMATION                                                  
226401     PERFORM IMS-GU-WDK601                                                
226501     IF SEGMENT-FINNS                                                     
226601       MOVE ART-IDFKNGRP     TO MOD-IDFKNGRP                              
226701       MOVE ART-KDPRODSL     TO MOD-KDPRODSL                              
226801       MOVE ART-TIFINLV      TO WS-DAPUBL                                 
226901       MOVE WS-DAPUBL        TO MOD-DAPUBL                                
227001       MOVE ART-TIURPROD     TO MOD-TIURPROD                              
227101                                                                          
227201*      ARTICLE C-LAGER INFO                                               
227301       PERFORM IMS-GU-WDK611                                              
227401       IF SEGMENT-FINNS                                                   
227501         PERFORM IMS-GNP-WDK629                                           
227601         IF SEGMENT-FINNS                                                 
227701           MOVE CREF-IDDC-REF  TO MOD-IDDC-REF                            
227801                                  W-IDDC-REF                              
227901                                  W-IDDC-B6                               
228001                                  WS-IDDC-REF                             
228101           MOVE CLAG-KDERS     TO MOD-KDERS                               
228201                                  WS-KDERS                                
228301           MOVE CLAG-REDIRLEV  TO MOD-REDIRLEV                            
228401           MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                            
228501           MOVE CLAG-KVQPACK-0 TO MOD-KVQPACK-0                           
228601           MOVE CLAG-KVQPACK-1 TO MOD-KVQPACK-1                           
228701           MOVE CLAG-KVQPACK-3 TO WS-KVQPACK-3                            
228801           MOVE CLAG-KVQPACK-4 TO MOD-KVQPACK-4                           
228901           MOVE CLAG-ADLAGOMR  TO MOD-ADLAGOMR                            
229001           MOVE CLAG-ADGANG    TO MOD-ADGANG                              
229101           MOVE CLAG-ADPLATS   TO MOD-ADPLATS                             
229201           MOVE CREF-FLWILSON  TO MOD-FLWILSON                            
229301           MOVE CREF-TIREFEFT  TO MOD-TIREFEFT                            
229401           MOVE CLAG-VKART     TO WS-VKART                                
229501           MOVE CLAG-VLARTNTO  TO WS-VLARTNTO                             
229601           MOVE CLAG-KVVORKO   TO WS-KVVORKO                              
229701*                                                                         
229801           PERFORM FL-CHECK-LOCAL-PARAMETERS                              
229901*                                                                         
230001           MOVE WS-VKART       TO MOD-VKART                               
230101           MOVE WS-VLARTNTO    TO MOD-VLARTNTO                            
230201           MOVE WS-KVQPACK-3   TO MOD-KVQPACK-3                           
230301*                                                                         
230401           PERFORM S11-CALC-AIR-COST                                      
230501*                                                                         
230601           IF SW-HAEMTA-INPUT-FAELT-JA                                    
230701              PERFORM FD-GET-FORECAST                                     
230801           END-IF                                                         
230901           PERFORM S10-GET-FC-TOT                                         
231001           PERFORM FJ-HANDLE-INPUT-FIELDS                                 
231101*                                                                         
231201           IF CREF-TIORDREG     > ZERO                                    
231301             MOVE CREF-TIORDREG                                           
231401                               TO MOD-TIORDREG                            
231501           ELSE                                                           
231601             MOVE MFS-RENSA-FAELT                                         
231701                               TO MOD-TIORDREG                            
231801           END-IF                                                         
231901*                                                                         
232001           PERFORM FC-GET-WDK9-INFO                                       
232101*                                                                         
232201           PERFORM FK-CHECK-SEASON                                        
232301*                                                                         
232401*                                                                         
232501           MOVE   'N'          TO MOD-SEASON-FCPL                         
232601           IF CLAG-DASEASON    >= DAGENS-DATUM-SEKEL                      
232701             MOVE 'Y'          TO MOD-SEASON-FCPL                         
232801           END-IF                                                         
232901*                                                                         
233001           IF CLAG-KDLEVSP > ZERO                                         
233101             MOVE 'F'          TO MOD-QUALBLOCK                           
233201           ELSE                                                           
233301             IF CLAG-KVSPARR-KVAL > ZERO                                  
233401               MOVE 'P'        TO MOD-QUALBLOCK                           
233501             ELSE                                                         
233601               MOVE '-'        TO MOD-QUALBLOCK                           
233701             END-IF                                                       
233801           END-IF                                                         
233901*                                                                         
234001           COMPUTE WS-SUPERWEEK ROUNDED =                                 
234101             (CLAG-KVTILLG-TOT + WS-PURCHQTY-SIM) /                       
234201             (UTIL-KVPB-TOT / 4.33)                                       
234301           ON SIZE ERROR                                                  
234401              MOVE +999        TO WS-SUPERWEEK                            
234501           END-COMPUTE                                                    
234601                                                                          
234701           IF WS-SUPERWEEK      > +999                                    
234801              MOVE +999        TO WS-SUPERWEEK                            
234901           ELSE                                                           
235001             IF WS-SUPERWEEK    < ZERO                                    
235101                MOVE ZERO      TO WS-SUPERWEEK                            
235201             END-IF                                                       
235301           END-IF                                                         
235401           IF WS-BALANCE        > ZERO                                    
235501           OR UTIL-KVPB-TOT     > ZERO                                    
235601              CONTINUE                                                    
235701           ELSE                                                           
235801              MOVE ZERO        TO WS-SUPERWEEK                            
235901           END-IF                                                         
236001           MOVE WS-SUPERWEEK   TO MOD-SUPERWEEK                           
236101*                                                                         
236201           PERFORM FF-GET-ERSATT-INFO                                     
236301*                                                                         
236401           PERFORM S2-LAES-WDA5A-KL1-EJ-REF                               
236501           MOVE WS-RESTKVANT                                              
236601                               TO MOD-CRIT-KVROS-NDC-CDC                  
                 PERFORM S12-LAES-WDA5-ENTER                                    
236501           MOVE WS-RESTKVANT                                              
236601                               TO MOD-KVROS-NDC-CDC                       
236701*                                                                         
236801           PERFORM FG-GET-WDL6-INFO                                       
236901*                                                                         
237001           PERFORM IMS-GU-WDK711                                          
237101           IF SEGMENT-FINNS                                               
237201              PERFORM FH-GET-WDK7-INFO                                    
237301           END-IF                                                         
237401*                                                                         
237501           PERFORM FA-BEHANDLA-ORDERINGGANG                               
237601*                                                                         
237701           PERFORM FI-GET-W6D1-PREADV-INFO                                
237801*                                                                         
237901           IF WS-KDERS          >  0                                      
238001             IF WS-KDERS        <  29                                     
238101               IF MOD-TEMFSFEL  = SPACE                                   
238201                   MOVE ARTIKEL-ERSATT                                    
238301                                     TO MED-IDMFSFEL                      
238401                   CALL WMEDKONV  USING MED-WMEDAREA                      
238501                   MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                      
238601               END-IF                                                     
238701             ELSE                                                         
238801               MOVE ARTIKEL-UTGANGEN                                      
238901                                     TO MED-IDMFSFEL                      
239001               CALL WMEDKONV      USING MED-WMEDAREA                      
239101               MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                      
239201             END-IF                                                       
239301           END-IF                                                         
239401                                                                          
239501         ELSE                                                             
239601           MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                            
239701           CALL WMEDKONV USING MED-WMEDAREA                               
239801           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
239901           PERFORM MFS-RENSA-FAELT-IN                                     
240001           PERFORM MFS-RENSA-FAELT-UT                                     
240101         END-IF                                                           
240201       ELSE                                                               
240301         MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                              
240401         CALL WMEDKONV USING MED-WMEDAREA                                 
240501         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
240601         PERFORM MFS-RENSA-FAELT-IN                                       
240701         PERFORM MFS-RENSA-FAELT-UT                                       
240801       END-IF                                                             
240901     ELSE                                                                 
241001       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
241101       CALL WMEDKONV USING MED-WMEDAREA                                   
241201       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
241301       PERFORM MFS-RENSA-FAELT-IN                                         
241401       PERFORM MFS-RENSA-FAELT-UT                                         
241501     END-IF                                                               
241601*                                                                         
241701     MOVE SPACE              TO MOD-BEART                                 
241801     PERFORM IMS-GU-WDD301-BSEQ                                           
241901     IF SEGMENT-FINNS                                                     
242001       MOVE 'GB'  TO W-IDSKYLT                                            
242101       PERFORM IMS-GNP-WDD311                                             
242201       IF SEGMENT-FINNS                                                   
242301         MOVE WDD311-TEXT-BEART                                           
242401                             TO MOD-BEART                                 
242501       END-IF                                                             
242601     END-IF                                                               
242701*                                                                         
242801     MOVE +1                 TO IX                                        
242901     PERFORM IMS-GU-WDN601                                                
243001     IF SEGMENT-FINNS                                                     
243101       PERFORM IMS-GNP-WDN611                                             
243201     END-IF                                                               
243301     PERFORM UNTIL SEGMENT-SAKNAS                                         
243401     OR              IX > 3                                               
243501       MOVE WDN6-KAT-BEMASTER (1:3)                                       
243601                             TO MOD-MODEL (IX)                            
243701       PERFORM IMS-GNP-WDN611                                             
243801       ADD +1                TO IX                                        
243901     END-PERFORM                                                          
244001*                                                                         
244101     IF WS-REF-KDREFTXT > ZERO                                            
244201       MOVE +1               TO IX                                        
244301       PERFORM UNTIL IX > REF-TEXT-TABMAX                                 
244401       OR WS-REF-KDREFTXT = REF-TEXT-KDREFTEXT (IX)                       
244501         ADD +1              TO IX                                        
244601       END-PERFORM                                                        
244701       IF IX > REF-TEXT-TABMAX                                            
244801         CONTINUE                                                         
244901       ELSE                                                               
245301         MOVE REF-TEXT (IX)    TO WS-TEMFSINF-NDC                         
245501       END-IF                                                             
245601     END-IF                                                               
245701*                                                                         
245801     IF WS-KVVORKO > ZERO                                                 
245901        MOVE 'VOR'           TO WS-TEMFSINF-TEXT                          
246001     END-IF                                                               
246101*                                                                         
246201*    CHECK SOURCING MARKET FOR THE PART                                   
246301     PERFORM FM-CHECK-ART-SOURCE                                          
246401*                                                                         
246501     IF INDATA-OK                                                         
246601     AND NOT MFS-UPDATE                                                   
246602     AND NOT MFS-UPD-V                                                    
246701       MOVE WS-TEMFSINF      TO MOD-TEMFSINF                              
246801     END-IF                                                               
246901*                                                                         
247001*    BELOW CODE IS TO HIGHLIGHT THE PURCHQTY AND FOR                      
247101*    CURSOR POSITIONING, AND NOT BEACUSE THERE IS SOME                    
247201*    ERROR IN THE FIELD                                                   
247301*                                                                         
247401     IF WS-PURCHQTY > ZERO                                                
247501       MOVE MFS-ALFA-FAELT-FEL                                            
247601                             TO MOD-PURCHQTY-ATTR                         
247701     END-IF                                                               
247801*                                                                         
247901*    AFTER UPDATE CURSOR POSITIONING ON PART NUMBER                       
248001*                                                                         
248101     IF MFS-UPDATE OR MFS-UPD-V                                           
248201       MOVE MFS-ADD-SAETT-CURSOR                                          
248301                             TO MOD-IDARTNR-IN-ATTR                       
248401     END-IF                                                               
248501     .                                                                    
248601     EJECT                                                                
248701                                                                          
248801 FA-BEHANDLA-ORDERINGGANG     SECTION.                                    
248901                                                                          
249001                                                                          
249101*    -- GET TOTAL INCOMING PROPOSALS FOR PREVIOUS YEAR                    
249201     MOVE DAGENS-AAR         TO MOD-IAAR                                  
249301     SUBTRACT 1 FROM DAGENS-AAR GIVING WS-FOREG-AAR                       
249401     MOVE WS-FOREG-AAR       TO MOD-FOREG-AAR                             
249501                                W-TIAAAA                                  
249601     MOVE ZERO               TO WS-KVOI-SUM                               
249701     PERFORM FAA-CALC-VV-I-PER-AA-1                                       
249801     PERFORM IMS-GU-WDL811                                                
249901                                                                          
250001*    -- GET TOTAL OI FOR PREVIOUS YEAR                                    
250101     IF SEGMENT-FINNS                                                     
250201        MOVE +1              TO IX                                        
250301        PERFORM UNTIL  IX     > WS-ANT-VV                                 
250401          ADD AAR-KVOI-PROG   (IX)                                        
250501                             TO WS-KVOI-SUM                               
250601          ADD AAR-KVOI-REFILL (IX)                                        
250701                             TO WS-KVOI-SUM                               
250801          ADD +1             TO IX                                        
250901        END-PERFORM                                                       
251001     END-IF                                                               
251101     MOVE WS-KVOI-SUM        TO MOD-KVOI-FOREG-AAR                        
251201                                                                          
251301*    -- COMPLETE WORKING TABLE WITH OI FOR PREV YEAR PER PERIOD           
251401                                                                          
251501     MOVE PER                TO IX                                        
251601     PERFORM UNTIL IX         > 12                                        
251701       MOVE WS-PER-STA-VV-1 (IX)                                          
251801                             TO IX-VV                                     
251901       PERFORM UNTIL IX-VV    > WS-PER-END-VV-1 (IX)                      
252001         IF SEGMENT-FINNS                                                 
252101            ADD AAR-KVOI-PROG   (IX-VV)                                   
252201                             TO WS-KVOI-VV-1 (IX)                         
252301            ADD AAR-KVOI-REFILL (IX-VV)                                   
252401                             TO WS-KVOI-VV-1 (IX)                         
252501         ELSE                                                             
252601            MOVE ZERO        TO WS-KVOI-VV-1 (IX)                         
252701         END-IF                                                           
252801         ADD +1           TO IX-VV                                        
252901       END-PERFORM                                                        
253001       ADD +1                TO IX                                        
253101     END-PERFORM                                                          
253201                                                                          
253301*  --- GET TOTAL INCOMING PROPOSALS FOR CURRENT YEAR                      
253401                                                                          
253501     MOVE ZERO               TO WS-KVOI-SUM                               
253601     MOVE +1                 TO IX                                        
253701     MOVE DAGENS-AAR         TO W-TIAAAA                                  
253801     PERFORM FAB-CALC-VV-I-PER-AA-0                                       
253901                                                                          
254001     PERFORM IMS-GU-WDL811                                                
254101     IF SEGMENT-FINNS                                                     
254201        IF DAGENS-PP > 1                                                  
254301           PERFORM UNTIL IX   > WS-PER-END-VV-0 (WS-ANT-PP)               
254401             ADD AAR-KVOI-PROG   (IX)                                     
254501                             TO WS-KVOI-SUM                               
254601             ADD AAR-KVOI-REFILL (IX)                                     
254701                             TO WS-KVOI-SUM                               
254801             ADD +1          TO IX                                        
254901           END-PERFORM                                                    
255001        END-IF                                                            
255101     END-IF                                                               
255201     MOVE WS-KVOI-SUM        TO MOD-KVOI-IAAR                             
255301                                                                          
255401*    -- COMPLETE WORKING TABLE WITH OI FOR CURR YEAR PER PERIOD           
255501                                                                          
255601     MOVE +1                 TO IX                                        
255701     PERFORM UNTIL IX         > DAGENS-PP                                 
255801       MOVE WS-PER-STA-VV-0 (IX)                                          
255901                             TO IX-VV                                     
256001       PERFORM UNTIL IX-VV    > WS-PER-END-VV-0 (IX)                      
256101         IF SEGMENT-FINNS                                                 
256201            ADD AAR-KVOI-PROG   (IX-VV)                                   
256301                             TO WS-KVOI-VV-0 (IX)                         
256401            ADD AAR-KVOI-REFILL (IX-VV)                                   
256501                             TO WS-KVOI-VV-0 (IX)                         
256601         ELSE                                                             
256701            MOVE ZERO        TO WS-KVOI-VV-0 (IX)                         
256801         END-IF                                                           
256901         ADD +1              TO IX-VV                                     
257001       END-PERFORM                                                        
257101       ADD +1                TO IX                                        
257201     END-PERFORM                                                          
257301                                                                          
257401*    --- WORKING TABLE FOR OI, SORTED, LATEST LAST                        
257501     MOVE WS-ANT-PP          TO IX                                        
257601     MOVE 12                 TO IX-VV                                     
257701     PERFORM UNTIL IX         = ZERO                                      
257801       MOVE WS-PER-TIAAPP-0 (IX)                                          
257901                             TO WS-PER-TIAAPP-S (IX-VV)                   
258001       MOVE WS-PER-STA-VV-0 (IX)                                          
258101                             TO WS-PER-STA-VV-S (IX-VV)                   
258201       MOVE WS-PER-END-VV-0 (IX)                                          
258301                             TO WS-PER-END-VV-S (IX-VV)                   
258401       MOVE WS-KVOI-VV-0    (IX)                                          
258501                             TO WS-KVOI-VV-S    (IX-VV)                   
258701       SUBTRACT  +1        FROM IX                                        
258801                                IX-VV                                     
258901     END-PERFORM                                                          
259001                                                                          
260701*    --- POPULATE SORTED TABLE WITH OI INFORMATION FROM                   
260801*    --- PREVIOUS YEAR                                                    
260901                                                                          
261001     IF IX-VV             NOT = ZERO                                      
261101        MOVE PER             TO IX                                        
261201        MOVE +1              TO IX-VV-1                                   
261301        PERFORM UNTIL IX-VV-1 > IX-VV                                     
261401          MOVE WS-PER-TIAAPP-1 (IX)                                       
261501                             TO WS-PER-TIAAPP-S (IX-VV-1)                 
261601          MOVE WS-PER-STA-VV-1 (IX)                                       
261701                             TO WS-PER-STA-VV-S (IX-VV-1)                 
261801          MOVE WS-PER-END-VV-1 (IX)                                       
261901                             TO WS-PER-END-VV-S (IX-VV-1)                 
262001          MOVE WS-KVOI-VV-1    (IX)                                       
262101                             TO WS-KVOI-VV-S    (IX-VV-1)                 
262201          ADD       +1       TO IX                                        
262301                                IX-VV-1                                   
262401        END-PERFORM                                                       
262501     END-IF                                                               
262601                                                                          
264301*    --- POPULATE OUTPUT FROM SORTED TABLE ON OI                          
264401                                                                          
264501     MOVE +1                 TO IX                                        
264601     MOVE +1                 TO MOD-IX                                    
264701     PERFORM UNTIL IX > +12                                               
264801       MOVE WS-PER-TIAAPP-S (IX)                                          
264901                             TO MOD-TIPP(MOD-IX)                          
265001       INSPECT MOD-TIPP(MOD-IX) REPLACING LEADING ZERO BY SPACE           
265101       MOVE WS-PER-STA-VV-S (IX)                                          
265201                             TO WS-FOM                                    
265301       MOVE WS-PER-END-VV-S (IX)                                          
265401                             TO WS-TOM                                    
265501       MOVE WS-FOM-TOM       TO MOD-TIVV-FOM-TOM(MOD-IX)                  
265601       MOVE WS-KVOI-VV-S (IX)                                             
265701                             TO MOD-KVOI-RULL(MOD-IX)                     
265801       ADD +1                TO IX                                        
265901                                MOD-IX                                    
266001     END-PERFORM                                                          
266101                                                                          
266201*    --- DISPLAY KVOI FOR THE CURRENT PERIOD                              
266301*                                                                         
266401     MOVE DAGENS-PP          TO IX                                        
266501     MOVE ZERO               TO WS-KVOI-SUM                               
266601     MOVE WS-ANTAL-VECKOR    TO MOD-VECKA                                 
266701     MOVE WS-KVOI-VV-0 (IX)  TO MOD-KVOI-INNEV                            
266801     .                                                                    
266901     EJECT                                                                
267001                                                                          
267101 FAA-CALC-VV-I-PER-AA-1 SECTION.                                          
267201                                                                          
267301     MOVE +1                 TO IX                                        
267401     MOVE DAGENS-PER         TO WS-TIAAPER                                
267501     SUBTRACT 1            FROM TIAA                                      
267601     PERFORM S3-CALC-VV-I-AA                                              
267701                                                                          
267801*    --- FILL IN WEEKS FOR DIFFERENT PERIODS                              
267901     MOVE TIAA               TO WS-TIAAPP-AA                              
268001     MOVE PER                TO WS-TIAAPP-PP                              
268101                                                                          
268201     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
268301                                                                          
268401       MOVE 'AARP'           TO DAT-KDDATFORM                             
268501       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
268601                                                                          
268701       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
268801                           DAT-O-TIDATUM DAT-KDSVAR                       
268901                                                                          
269001       IF DAT-KDSVAR-OK                                                   
269101          MOVE DAT-TIVV      TO WS-PER-STA-VV-1(WS-TIAAPP-PP)             
269201          MOVE WS-TIAAPP-PP                                               
269301                             TO WS-PER-TIAAPP-1(WS-TIAAPP-PP)             
269401                                                                          
269501       ELSE                                                               
269601          STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-1'                     
269701          DELIMITED BY SIZE INTO FELTEXT                                  
269801          CALL FELLOG                                                     
269901       END-IF                                                             
270001                                                                          
270101       ADD 1                 TO WS-TIAAPP                                 
270201     END-PERFORM                                                          
270301                                                                          
270401     MOVE +1                 TO IX                                        
270501                                                                          
270601     PERFORM UNTIL IX > +11                                               
270701                                                                          
270801       COMPUTE WS-PER-END-VV-1 (IX) =                                     
270901               WS-PER-STA-VV-1 (IX + 1) - 1                               
271001                                                                          
271101       ADD +1                TO IX                                        
271201     END-PERFORM                                                          
271301                                                                          
271401     MOVE WS-ANT-VV          TO WS-PER-END-VV-1 (12)                      
271501     .                                                                    
271601     EJECT                                                                
271701                                                                          
271801 FAB-CALC-VV-I-PER-AA-0     SECTION.                                      
271901                                                                          
272001     MOVE +1                 TO IX                                        
272101     MOVE DAGENS-PER         TO WS-TIAAPER                                
272201     PERFORM S3-CALC-VV-I-AA                                              
272301*                                                                         
272401     MOVE DAGENS-DATUM (1:2) TO WS-TIAAPP-AA                              
272501     MOVE 1                  TO WS-TIAAPP-PP                              
272601                                                                          
272701     PERFORM UNTIL WS-TIAAPP-PP > 12                                      
272801                                                                          
272901       MOVE 'AARP'           TO DAT-KDDATFORM                             
273001       MOVE WS-TIAAPP        TO DAT-I-TIDATUM                             
273101                                                                          
273201       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
273301                           DAT-O-TIDATUM DAT-KDSVAR                       
273401                                                                          
273501       IF DAT-KDSVAR-OK                                                   
273601*--- START WEEK IS WEEK 1 OF THE CURRENT/NEW YEAR                         
273701         IF DAT-TIVV = +52 OR +53                                         
273801            MOVE 1           TO WS-PER-STA-VV-0(WS-TIAAPP-PP)             
273901                                WS-PER-TIAAPP-0(WS-TIAAPP-PP)             
274001         ELSE                                                             
274101            MOVE DAT-TIVV    TO WS-PER-STA-VV-0(WS-TIAAPP-PP)             
274201            MOVE WS-TIAAPP-PP                                             
274301                             TO WS-PER-TIAAPP-0(WS-TIAAPP-PP)             
274401         END-IF                                                           
274501                                                                          
274601       ELSE                                                               
274701           STRING ' FEL FRÅN DATUMRUTIN WDATKONV AA-0'                    
274801           DELIMITED BY SIZE INTO FELTEXT                                 
274901           CALL FELLOG                                                    
275001       END-IF                                                             
275101                                                                          
275201       ADD 1                 TO WS-TIAAPP                                 
275301     END-PERFORM                                                          
275401     COMPUTE WS-ANT-PP        = DAGENS-PP - 1                             
275501                                                                          
275601     MOVE +1                 TO IX                                        
275701                                                                          
275801     PERFORM UNTIL IX > 11                                                
275901                                                                          
276001       COMPUTE WS-PER-END-VV-0 (IX) =                                     
276101               WS-PER-STA-VV-0 (IX + 1) - 1                               
276201                                                                          
276301       ADD +1                TO IX                                        
276401     END-PERFORM                                                          
276501     MOVE WS-ANT-VV          TO WS-PER-END-VV-0(12)                       
276601     .                                                                    
276701     EJECT                                                                
276801                                                                          
280501 FC-GET-WDK9-INFO    SECTION.                                             
280601                                                                          
280701*        ARTIKELREGISTER ORDER ENTRY                                      
280801     PERFORM IMS-GU-WDK901                                                
280901     IF SEGMENT-FINNS                                                     
281001        COMPUTE WS-KVOKS-TOT = WDK9-ART-KVOKS-BULK +                      
281101                               WDK9-ART-KVOKS-DAG  +                      
281201                               WDK9-ART-KVOKS-VOR                         
281301     ELSE                                                                 
281401        MOVE ZERO           TO WS-KVOKS-TOT                               
281501     END-IF                                                               
281601                                                                          
281701     COMPUTE WS-BALANCE ROUNDED =                                         
281801          (CLAG-KVLS - CLAG-KVROS - WS-KVOKS-TOT - CLAG-KVRESS)           
281901     COMPUTE WS-ORDERED ROUNDED =                                         
282001                 (CLAG-KVBEART + CLAG-KVAKS-PAV + CLAG-KVAKS-T)           
282101*                                                                         
282201     MOVE CLAG-KVROS        TO WS-KVROS-CDC                               
282301     MOVE WS-KVROS-CDC      TO MOD-KVROS                                  
282401     MOVE WS-BALANCE        TO MOD-BALANCE                                
282501     MOVE CLAG-KVAKS-CDC    TO MOD-KVAKS-CDC                              
282601     MOVE WS-ORDERED        TO MOD-ORDERED                                
282701     .                                                                    
282801     EJECT                                                                
282901                                                                          
283001 FD-GET-FORECAST     SECTION.                                             
283101                                                                          
283201     MOVE CLAG-KVPB-SEP      TO WS-RED-KVPB-SEP                           
283301     MOVE WS-RED-KVPB-SEP    TO MOD-KVPB-SEP                              
283401     MOVE MFS-ADD-LAES-IN-FAELT                                           
283501                             TO MOD-KVPB-SEP-ATTR                         
283601     .                                                                    
283701     EJECT                                                                
283801                                                                          
283901 FF-GET-ERSATT-INFO    SECTION.                                           
284001                                                                          
284101     MOVE MFS-RENSA-FAELT    TO MOD-REPLACES                              
284201                                                                          
284301     IF ART-FLERS             = JA                                        
284401        MOVE ART-IDARTNR     TO W-IDARTNR-MIN7                            
284501                                W-IDARTNR-MAX7                            
284601        PERFORM IMS-GU-WDD7-WDD7A-MINMAX                                  
284701        IF SEGMENT-FINNS                                                  
284801           IF WDD7A1-ERS-IDARTNR NOT = ZERO                               
284901              MOVE WDD7A1-ERS-IDARTNR                                     
285001                             TO MOD-REPLACES                              
285101              INSPECT MOD-REPLACES REPLACING                              
285201                                   LEADING ZERO BY SPACE                  
285301              PERFORM IMS-GN-WDD7-WDD7A-MINMAX                            
285401              IF SEGMENT-FINNS                                            
285501              AND WDD7A1-ERS-IDARTNR NOT = ZERO                           
285601                MOVE 'VARIOUS'                                            
285701                             TO MOD-REPLACES                              
285801              END-IF                                                      
285901           END-IF                                                         
286001        END-IF                                                            
286101     END-IF                                                               
286201*                                                                         
286301*                                                                         
286401     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
286501                                MOD-TIERSDAT-PREL                         
286601     PERFORM IMS-GU-WDD701                                                
286701     IF SEGMENT-FINNS                                                     
286801        PERFORM IMS-GNP-WDD702                                            
286901        IF SEGMENT-FINNS                                                  
287001           MOVE WDD702-IDARTNR-TILLK                                      
287101                             TO MOD-REPL-BY                               
287201           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
287301           PERFORM IMS-GNP-WDD702                                         
287401           IF SEGMENT-FINNS                                               
287501             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
287601           END-IF                                                         
287701        END-IF                                                            
287801        IF  WS-KDERS (3:1)    > 0                                         
287901        AND WS-KDERS (3:1)    < 7                                         
288001          PERFORM IMS-GNP-WDD704                                          
288101          IF SEGMENT-FINNS                                                
288201             MOVE WDD704-TIERSDAT-PREL-C1                                 
288301                             TO WS-TIERSDAT-PREL                          
288401             MOVE WS-TIERSDAT-AAVV                                        
288501                             TO MOD-TIERSDAT-PREL                         
288601          END-IF                                                          
288701        END-IF                                                            
288801     END-IF                                                               
288901     .                                                                    
289001     EJECT                                                                
289101                                                                          
289201 FG-GET-WDL6-INFO SECTION.                                                
289301                                                                          
289401*    --- WE FETCH THE QUANTITY AND ETA INFORMATION BELOW                  
289501*    --- PLUS LATEST R32                                                  
289601     PERFORM IMS-GU-WDL601                                                
289701     IF SEGMENT-FINNS                                                     
289801       PERFORM IMS-GNP-WDL611                                             
289901       MOVE ZERO             TO WS-TIINLINL                               
290001       PERFORM UNTIL SEGMENT-SAKNAS                                       
290101         IF WDL6-INL-IDDC     = WS-IDDC-CDC                               
290201           MOVE WDL6-INL-TIINLINL                                         
290301                             TO TMP1-YYMMDD                               
290401           MOVE WS-TIINLINL                                               
290501                             TO TMP2-YYMMDD                               
290601           PERFORM WY2000P1                                               
290701           IF TMP1-YYMMDD     > TMP2-YYMMDD                               
290801             MOVE WDL6-INL-TIINLINL                                       
290901                             TO WS-TIINLINL                               
291001           END-IF                                                         
291101           IF (WDL6-INL-IDPTYP = '310' OR 'R31' OR 'R30')                 
291201           AND(WDL6-INL-KDRT  = ZERO)                                     
291301             MOVE WDL6-INL-TIBERANK                                       
291401                             TO TMP1-YYMMDD                               
291501             MOVE WS-SECOND-ETA                                           
291601                             TO TMP2-YYMMDD                               
291701             PERFORM WY2000P1                                             
291901             IF TMP1-YYMMDD   > TMP2-YYMMDD                               
292001             AND WS-SECOND-ETA NOT = 999999                               
292101               CONTINUE                                                   
292201             ELSE                                                         
292301               MOVE WDL6-INL-TIBERANK                                     
292401                             TO TMP1-YYMMDD                               
292501               MOVE WS-FIRST-ETA                                          
292601                             TO TMP2-YYMMDD                               
292701               PERFORM WY2000P1                                           
292901               IF (TMP1-YYMMDD < TMP2-YYMMDD                              
293001               AND WS-FIRST-ETA NOT = 999999)                             
293101               OR WS-FIRST-ETA = 999999                                   
293201                 MOVE WS-FIRST-ETA TO WS-SECOND-ETA                       
293301                 MOVE WS-FIRST-ETA-QTY                                    
293401                                   TO WS-SECOND-ETA-QTY                   
293501                 MOVE ZERO         TO WS-FIRST-ETA                        
293601                                      WS-FIRST-ETA-QTY                    
293701                 MOVE WDL6-INL-TIBERANK                                   
293801                                   TO WS-FIRST-ETA                        
293901                 MOVE WDL6-INL-KVAVIS                                     
294001                                   TO WS-FIRST-ETA-QTY                    
294101               ELSE                                                       
294201                 IF WDL6-INL-TIBERANK = WS-FIRST-ETA                      
294301                   ADD WDL6-INL-KVAVIS                                    
294401                                   TO WS-FIRST-ETA-QTY                    
294501                 ELSE                                                     
294601                   IF WDL6-INL-TIBERANK = WS-SECOND-ETA                   
294701                     ADD WDL6-INL-KVAVIS                                  
294801                                   TO WS-SECOND-ETA-QTY                   
294901                   ELSE                                                   
295001                     MOVE WDL6-INL-TIBERANK                               
295101                                   TO WS-SECOND-ETA                       
295201                     MOVE WDL6-INL-KVAVIS                                 
295301                                   TO WS-SECOND-ETA-QTY                   
295401                   END-IF                                                 
295501                 END-IF                                                   
295601               END-IF                                                     
295701             END-IF                                                       
295801           END-IF                                                         
295901         END-IF                                                           
296001         PERFORM IMS-GNP-WDL611                                           
296101       END-PERFORM                                                        
296201       MOVE WS-TIINLINL          TO MOD-TIINLINL                          
296301                                                                          
296401       IF  WS-FIRST-ETA           < 999999                                
296501       AND WS-FIRST-ETA           > ZERO                                  
296601         MOVE WS-FIRST-ETA       TO MOD-TIBERANK(1)                       
296701       ELSE                                                               
296801         MOVE MFS-RENSA-FAELT    TO MOD-TIBERANK(1)                       
296901       END-IF                                                             
297001       IF WS-SECOND-ETA           < 999999                                
297101       AND WS-SECOND-ETA          > ZERO                                  
297201         MOVE WS-SECOND-ETA      TO MOD-TIBERANK(2)                       
297301       ELSE                                                               
297401         MOVE MFS-RENSA-FAELT    TO MOD-TIBERANK(2)                       
297501       END-IF                                                             
297601       IF WS-FIRST-ETA-QTY        > 0                                     
297701         MOVE WS-FIRST-ETA-QTY   TO MOD-KVAVIS-SUM(1)                     
297801       ELSE                                                               
297901         MOVE MFS-RENSA-FAELT    TO MOD-KVAVIS-SUM(1)                     
298001       END-IF                                                             
298101       IF WS-SECOND-ETA-QTY       > 0                                     
298201         MOVE WS-SECOND-ETA-QTY  TO MOD-KVAVIS-SUM(2)                     
298301       ELSE                                                               
298401         MOVE MFS-RENSA-FAELT    TO MOD-KVAVIS-SUM(2)                     
298501       END-IF                                                             
298601     END-IF                                                               
298701     .                                                                    
298801     EJECT                                                                
298901                                                                          
299001 FH-GET-WDK7-INFO SECTION.                                                
299101                                                                          
299201     MOVE  ZERO              TO WS-INDATE                                 
299301*  ----  GET LAST PUBWEEK FOR DC 71. IF ZERO GET PUBWEEK                  
299401*  ----  FROM K6                                                          
299501     PERFORM IMS-GU-WDK712                                                
299601     IF SEGMENT-FINNS                                                     
299701        MOVE LART-DAPUBL     TO WS-INDATE                                 
299801     END-IF                                                               
299901                                                                          
300001     IF WS-INDATE-6 > ZERO                                                
300101        MOVE 'AAMMDD'        TO DAT-KDDATFORM                             
300201        MOVE WS-INDATE-6     TO DAT-I-TIDATUM                             
300301                                                                          
300401        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
300501                            DAT-O-TIDATUM DAT-KDSVAR                      
300601                                                                          
300701        IF DAT-KDSVAR-OK                                                  
300801           MOVE DAT-TIAAVVD  TO MOD-TIFINLV                               
300901        ELSE                                                              
301001           STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS              
301101           DELIMITED BY SIZE INTO FELTEXT                                 
301201           CALL FELLOG                                                    
301301        END-IF                                                            
301401     ELSE                                                                 
301501        MOVE ART-TIFINLV     TO MOD-TIFINLV                               
301601     END-IF                                                               
301701                                                                          
301801     COMPUTE WS-AVAILABLE ROUNDED                                         
301901                              = SLAG-KVLS       -                         
302001                                SLAG-KVRESS     -                         
302101                                SLAG-KVOKS-BULK -                         
302201                                SLAG-KVOKS-DAG                            
302301     COMPUTE WS-KVROS-SDC ROUNDED                                         
302401                              = SLAG-KVROS-BULK +                         
302501                                SLAG-KVROS-DAG                            
302601     COMPUTE WS-KVPB-SDC      = SLAG-KVPB-REF   +                         
302701                                SLAG-KVPBREOI                             
302801     COMPUTE WS-PREADV-QTY    = SLAG-KVAKS-PAV  +                         
302901                                SLAG-KVBEART                              
303001*                                                                         
303101     MOVE SLAG-IDDC-REF      TO WS-IDDC-REF-K7                            
303201     MOVE WS-AVAILABLE       TO MOD-AVAIL                                 
303301     MOVE SLAG-KVAKS-SDC     TO MOD-KVAKS-SDC                             
303401     MOVE WS-KVROS-SDC       TO MOD-KVROS-SDC                             
303501     MOVE WS-KVPB-SDC        TO MOD-KVPB-SDC                              
303601*    MOVE SLAG-KVROS-DAG     TO MOD-KVROS-NDC-CDC                         
303701                                                                          
303801     IF SLAG-FLPB-FLYTT = JA                                              
303901       MOVE 'REPL'           TO WS-TEMFSINF-NDC                           
304001     END-IF                                                               
304101                                                                          
304201*  ----  GET MOQ/MINIMUM QUANTITY FOR THE PART                            
304301     PERFORM IMS-GU-WDK722                                                
304401     IF SEGMENT-FINNS                                                     
304501        MOVE XLAG-KVPALL     TO MOD-KVPALL                                
304601     ELSE                                                                 
304701        MOVE ZERO            TO MOD-KVPALL                                
304801     END-IF                                                               
304901     .                                                                    
305001     EJECT                                                                
305101                                                                          
305201 FI-GET-W6D1-PREADV-INFO SECTION.                                         
305301                                                                          
305401     MOVE ZERO               TO WS-KVAVIS                                 
305501     IF WS-IDDC-REF-K7    NOT > SPACES                                    
305601        MOVE W-IDARTNR          TO W-IDARTNR-HSEQ                         
305701        PERFORM IMS-GN-W6D111-W6D1SEQ                                     
305801        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
305901          IF  W6D1-ART-IDDC   = W-IDDC-REF                                
306001          AND W6D1-ART-IDLOPNRM                                           
306101                              = ZERO                                      
306201          AND W6D1-ART-FLFEL  = NEJ                                       
306301              ADD W6D1-ART-KVAVIS                                         
306401                             TO WS-KVAVIS                                 
306501          END-IF                                                          
306601          PERFORM IMS-GN-W6D111-W6D1SEQ                                   
306701        END-PERFORM                                                       
306801     ELSE                                                                 
306901        MOVE WS-PREADV-QTY   TO WS-KVAVIS                                 
307001     END-IF                                                               
307101     MOVE WS-KVAVIS          TO MOD-KVAVIS                                
307201     .                                                                    
307301     EJECT                                                                
307401                                                                          
307501 FJ-HANDLE-INPUT-FIELDS  SECTION.                                         
307601                                                                          
307701     MOVE CLAG-KVQ           TO MOD-KVREFBER                              
307801     MOVE CREF-KVREFPKT      TO MOD-KVREFPKT                              
307901*                                                                         
308001*                                                                         
308101     IF SW-ALW-AIR-SAME-JA                                                
308201        PERFORM S4-GET-ALW-AIR                                            
308301        MOVE WS-FLAGGA-FCD-S TO MOD-FLAGGA-FCD                            
308401        MOVE MFS-ADD-LAES-IN-FAELT                                        
308501                             TO MOD-FLAGGA-FCD-ATTR                       
308601     END-IF                                                               
308701*                                                                         
308801     IF SW-AUTO-REF-SAME-JA                                               
308901        PERFORM S5-GET-AUTO-REF                                           
309001        MOVE WS-FLREFBEO-S   TO MOD-FLREFBEO                              
309101        MOVE MFS-ADD-LAES-IN-FAELT                                        
309201                             TO MOD-FLREFBEO-ATTR                         
309301     END-IF                                                               
309401*                                                                         
309501     IF SW-COMMENT-SAME-JA                                                
309601        PERFORM S6-GET-COMMENT                                            
309701        IF WS-COMMENT-1       > SPACES                                    
309801           MOVE WS-COMMENT-1 TO MOD-COMMENT (1)                           
309901        ELSE                                                              
310001           MOVE MFS-RENSA-FAELT                                           
310101                             TO MOD-COMMENT (1)                           
310201        END-IF                                                            
310301        IF WS-COMMENT-2       > SPACES                                    
310401           MOVE WS-COMMENT-2 TO MOD-COMMENT (2)                           
310501        ELSE                                                              
310601           MOVE MFS-RENSA-FAELT                                           
310701                             TO MOD-COMMENT (2)                           
310801        END-IF                                                            
310901     END-IF                                                               
311001     .                                                                    
311101     EJECT                                                                
311201                                                                          
311301 FK-CHECK-SEASON SECTION.                                                 
311401                                                                          
311501     MOVE +1                TO IX                                         
311601        MOVE NEJ            TO SW-SEASON                                  
311701     PERFORM IMS-GU-WDK626                                                
311801     IF SEGMENT-FINNS                                                     
311901        PERFORM UNTIL IX     > 12                                         
312001          MOVE JUST-RESEASON(IX)                                          
312101                            TO WS-RESEASON-PLAN(IX)                       
312201          ADD +1            TO IX                                         
312301        END-PERFORM                                                       
312401*                                                                         
312501        MOVE +1             TO IX                                         
312601        PERFORM UNTIL IX     > +12                                        
312701        OR SW-SEASON-JA                                                   
312801           IF WS-RESEASON-PLAN(IX)                                        
312901                         NOT = +1.00                                      
313001               MOVE JA      TO SW-SEASON                                  
313101           END-IF                                                         
313201          ADD  +1           TO IX                                         
313301        END-PERFORM                                                       
313302        IF JUST-KVPB-JUST(1) > ZERO                                       
313303          MOVE MFS-ADD-LYS-UPP-FAELT                                      
313304                                 TO MOD-KVREFPKT-ATTR                     
313305        END-IF                                                            
313306                                                                          
313401     END-IF                                                               
313501*                                                                         
313601     IF SW-SEASON-JA                                                      
313701       MOVE 'Y'             TO MOD-SEASON                                 
313801     ELSE                                                                 
313901       MOVE 'N'             TO MOD-SEASON                                 
314001     END-IF                                                               
314101     .                                                                    
314201     EJECT                                                                
314301                                                                          
314401 FL-CHECK-LOCAL-PARAMETERS SECTION.                                       
314501                                                                          
314601**HÄMTAR LOKALA PARAMETRAR FRÅN REFILLANDE DC                             
314701     PERFORM S9-SEARCH-IDLAND                                             
314801     PERFORM IMS-GU-WDK712                                                
314901     IF SEGMENT-FINNS                                                     
315001       IF LART-VKART > 0                                                  
315101         MOVE LART-VKART             TO WS-VKART                          
315201       END-IF                                                             
315301       IF LART-VLARTNTO > 0                                               
315401         MOVE LART-VLARTNTO          TO WS-VLARTNTO                       
315501       END-IF                                                             
315601       IF LART-KVQPACK-3 > 0                                              
315701         MOVE LART-KVQPACK-3         TO WS-KVQPACK-3                      
315801       END-IF                                                             
315901     END-IF                                                               
316001                                                                          
316101     .                                                                    
316201     EJECT                                                                
316301 FM-CHECK-ART-SOURCE SECTION.                                             
316401                                                                          
316501*  ----  CHECK MARKET WHERE PART IS SOURCED                               
316601*  ----  FOR CHECKING SOURCE MARKET CALL W271UTIL WITH KDCAL 001          
316701                                                                          
316801     INITIALIZE  UTIL-W271UTIL                                            
316901     MOVE 001                   TO UTIL-KDCALL                            
317001     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
317101                                                                          
317201     CALL W271UTIL USING UTIL-W271UTIL                                    
317301                         UTIL-WDK6-PCB                                    
317401                         UTIL-WDK7-PCB                                    
317501                         UTIL-WDB6-PCB                                    
317601                                                                          
317701     IF UTIL-KDSVAR-OK                                                    
317801        MOVE UTIL-TEXT          TO WS-TEMFSINF-SOURCE                     
317901     END-IF                                                               
318001     .                                                                    
318101     EJECT                                                                
318201                                                                          
318301 H-UPD-WDK6 SECTION.                                                      
318401                                                                          
318501     INITIALIZE REFL-W272REFL                                             
318601     PERFORM IMS-GU-WDK611                                                
318701     IF SEGMENT-FINNS                                                     
318801       PERFORM HA-UPPDATERA-WDK611-25-29                                  
318901     END-IF                                                               
319001     .                                                                    
319101     EJECT                                                                
319201                                                                          
319301 HA-UPPDATERA-WDK611-25-29 SECTION.                                       
319401                                                                          
319501*    IF (WS-KVPB-SEP         > ZERO)                                      
319601     IF  SW-KVPB-SEP-JA                                                   
319701     OR (MID-COMMENT-1  NOT  = ALL '+')                                   
319801     OR (MID-COMMENT-2  NOT  = ALL '+')                                   
319901     OR (MID-FLAGGA-FCD NOT  = ALL '+')                                   
320001     OR (MID-FLREFBEO   NOT  = ALL '+')                                   
320101                                                                          
320201       IF MID-COMMENT-1 NOT  = ALL '+'                                    
320301       OR MID-COMMENT-2 NOT  = ALL '+'                                    
320401          MOVE 1             TO W-KDNOTTYP                                
320501          PERFORM IMS-GHU-WDK625                                          
320601          MOVE SPACES            TO NOT-TEARTNOT                          
320701          IF MID-COMMENT-1        > SPACES                                
320801             MOVE MID-COMMENT-1  TO NOT-TEARTNOT  (1:36)                  
320901             IF SEGMENT-FINNS                                             
321001                PERFORM IMS-REPL-WDK625                                   
321101             ELSE                                                         
321201               IF SEGMENT-SAKNAS                                          
321301                  MOVE  1        TO NOT-KDNOTTYP                          
321401                  PERFORM IMS-ISRT-WDK625                                 
321501               END-IF                                                     
321601             END-IF                                                       
321701          ELSE                                                            
321801            IF MID-COMMENT-1  NOT > SPACES                                
321901              IF SEGMENT-FINNS                                            
322001                 PERFORM IMS-DLET-WDK625                                  
322101              END-IF                                                      
322201            END-IF                                                        
322301          END-IF                                                          
322401*                                                                         
322501          MOVE 2                 TO W-KDNOTTYP                            
322601          PERFORM IMS-GHU-WDK625                                          
322701          MOVE SPACES            TO NOT-TEARTNOT                          
322801          IF MID-COMMENT-2        > SPACES                                
322901             MOVE MID-COMMENT-2  TO NOT-TEARTNOT  (1:36)                  
323001             IF SEGMENT-FINNS                                             
323101                PERFORM IMS-REPL-WDK625                                   
323201             ELSE                                                         
323301               IF SEGMENT-SAKNAS                                          
323401                  MOVE  2        TO NOT-KDNOTTYP                          
323501                  PERFORM IMS-ISRT-WDK625                                 
323601               END-IF                                                     
323701             END-IF                                                       
323801          ELSE                                                            
323901            IF MID-COMMENT-2  NOT > SPACES                                
324001              IF SEGMENT-FINNS                                            
324101                 PERFORM IMS-DLET-WDK625                                  
324201              END-IF                                                      
324301            END-IF                                                        
324401          END-IF                                                          
324501       END-IF                                                             
324601*                                                                         
324701       PERFORM IMS-GHU-WDK61129                                           
324801       IF WS-FLAGGA-FCD       NOT = SPACE                                 
324901          MOVE WS-FLAGGA-FCD     TO K6-CREF-FLFLYG                        
325001*To exclude LOCK logic for the new buyer steering rule                    
325101          IF DCS-KDDCSTYR-BUY > ZERO                                      
325201*                                                                         
325301             IF WS-FLAGGA-FCD        = JA                                 
325401                MOVE 1              TO K6-CREF-IDREFTAB                   
325501                MOVE 'J'            TO K6-CREF-FLTABUPD                   
325601             ELSE                                                         
325701               IF WS-FLAGGA-FCD      = NEJ OR 'S'                         
325801                  MOVE ZERO         TO K6-CREF-IDREFTAB                   
325901                  MOVE 'J'          TO K6-CREF-FLTABUPD                   
326001               END-IF                                                     
326101             END-IF                                                       
326201          END-IF                                                          
326301       END-IF                                                             
326401*                                                                         
326501       IF MID-FLREFBEO            = ALL '+'                               
326601          CONTINUE                                                        
326701       ELSE                                                               
326801          IF MID-FLREFBEO = YES                                           
326901            MOVE JA              TO K6-CREF-FLREFBEO                      
327001          ELSE                                                            
327101            MOVE MID-FLREFBEO    TO K6-CREF-FLREFBEO                      
327201          END-IF                                                          
327301       END-IF                                                             
327401*                                                                         
327501*      IF WS-KVPB-SEP             = ZERO                                  
327601       IF SW-KVPB-SEP-NEJ                                                 
327701          CONTINUE                                                        
327801       ELSE                                                               
327901          IF WS-KVPB-SEP          > K6-CLAG-KVPB-SEP                      
328001             MOVE NEJ            TO K6-CREF-FLREFNYO                      
328101          END-IF                                                          
328201          MOVE WS-TIPBDAT        TO K6-CLAG-TIPBDAT                       
328301          MOVE WS-KVPB-SEP       TO K6-CLAG-KVPB-SEP                      
328401                                    K6-CLAG-KVPB-HIST                     
328501       END-IF                                                             
328601       PERFORM IMS-REPL-WDK61129                                          
328701       MOVE JA                   TO SW-COMMENT-SAME                       
328801                                    SW-ALW-AIR-SAME                       
328901                                    SW-AUTO-REF-SAME                      
329001       PERFORM HAA-RECALCULATE                                            
329101     END-IF                                                               
329201     .                                                                    
329301     EJECT                                                                
329401                                                                          
329501 HAA-RECALCULATE SECTION.                                                 
329601                                                                          
329701*    IF WS-KVPB-SEP               > ZERO                                  
329801     IF SW-KVPB-SEP-JA                                                    
329901        PERFORM IMS-GHU-WDK61129                                          
330001        MOVE  WS-TIPBDAT         TO K6-CLAG-TIPBDAT                       
330101        IF K6-CLAG-DAPBPLAN      >= DAGENS-DATUM-SEKEL                    
330201           MOVE K6-CLAG-KVPB-PLAN                                         
330301                                 TO WS-KVPB-PLAN                          
330401        ELSE                                                              
330501           MOVE W-IDARTNR        TO PBTO-IDARTNR                          
330601           CALL W222PBTO      USING PBTO-W222PBTO                         
330701                                    PBTO-WDK6-PCB                         
330801                                    PBTO-WDK7-PCB                         
330901                                    PBTO-ARTM-PCB                         
331001                                    PBTO-2501-PCB                         
331101                                    PBTO-WDB6R-PCB                        
331201                                    PBTO-WDK7R-PCB                        
331301                                    PBTO-WDB6-PCB                         
331401                                    PBTO-WDD7-PCB                         
331501                                    PBTO-WDK7E-PCB                        
331601                                    PBTO-W222-UTIL-WDK6-PCB               
331701                                    PBTO-W222-UTIL-WDK7-PCB               
331801                                    PBTO-W222-UTIL-WDB6-PCB               
331901                                    PBTO-W222-UTUP-WDK7-PCB               
332001                                    PBTO-W222-UTUP-WDB6-PCB               
332101                                    PBTO-W222-UTUP-UTIL-WDK6-PCB          
332201                                    PBTO-W222-UTUP-UTIL-WDK7-PCB          
332301                                    PBTO-W222-UTUP-UTIL-WDB6-PCB          
332401           IF PBTO-KDSVAR = JA                                            
332501              MOVE PBTO-KVPB-PLAN                                         
332601                                 TO WS-KVPB-PLAN                          
332701                                    K6-CREF-KVPB-PLAN                     
332801           END-IF                                                         
332901        END-IF                                                            
333001                                                                          
333101        MOVE W-IDARTNR           TO REFL-IDARTNR                          
333201        MOVE W-IDDC              TO REFL-IDDC                             
333301        MOVE K6-CREF-IDDC-REF    TO REFL-IDDC-REF                         
333401        MOVE K6-CREF-IDREFTAB    TO REFL-IDREFTAB                         
333501        MOVE K6-CREF-FLREFBEO    TO REFL-FLREFBEO                         
333601        MOVE K6-CREF-FLWILSON    TO REFL-FLWILSON                         
333701        MOVE K6-CLAG-PRARTSTD    TO REFL-PRARTBES                         
333801        MOVE K6-CREF-FLFLYG      TO REFL-FLFLYG                           
333901                                                                          
334001        MOVE K6-CREF-TIREFPKT    TO TMP1-YYMMDD                           
334101        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
334201        PERFORM WY2000P1                                                  
334301                                                                          
334401        IF TMP1-YYMMDD >= TMP2-YYMMDD                                     
334501          MOVE K6-CREF-KVREFPKT  TO REFL-IN-KVREFPKT                      
334601        ELSE                                                              
334701          MOVE ZERO              TO REFL-IN-KVREFPKT                      
334801        END-IF                                                            
334901                                                                          
335001        MOVE K6-CREF-TIREFPAF    TO TMP1-YYMMDD                           
335101        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
335201        PERFORM WY2000P1                                                  
335301                                                                          
335401        IF TMP1-YYMMDD           >= TMP2-YYMMDD                           
335501          MOVE K6-CLAG-KVQ       TO REFL-IN-KVREFBER                      
335601        ELSE                                                              
335701          MOVE ZERO              TO REFL-IN-KVREFBER                      
335801        END-IF                                                            
335901                                                                          
336001        MOVE 2                  TO W272-UTUP-KDCALL                       
336101        MOVE W-IDARTNR          TO W272-UTUP-IDARTNR                      
336201        MOVE W-IDDC             TO W272-UTUP-IDDC                         
336301        IF K6-CREF-IDDC-REF = SPACE                                       
336401          CALL FELLOG                                                     
336501        ELSE                                                              
336601          MOVE K6-CREF-IDDC-REF TO W272-UTUP-IDDC-REF                     
336701        END-IF                                                            
336801                                                                          
336901        CALL W272UTUP USING W272-UTUP-W272UTUP                            
337001                            U2-WDK6-PCB                                   
337101                            U2-WDB6-PCB                                   
337201                            U2-PBTO-W222-WDK6-PCB                         
337301                            U2-PBTO-W222-WDK7-PCB                         
337401                            U2-PBTO-W222-ARTM-PCB                         
337501                            U2-PBTO-W222-REFL1-2501-PCB                   
337601                            U2-PBTO-W222-REFL1-WDB6R-PCB                  
337701                            U2-PBTO-W222-REFL1-WDK7R-PCB                  
337801                            U2-PBTO-W222-WDB6-PCB                         
337901                            U2-PBTO-W222-WDD7-PCB                         
338001                            U2-PBTO-W222-WDK7E-PCB                        
338101                            U2-PBTO-W222-REFL1-UTIL-K6-PCB                
338201                            U2-PBTO-W222-REFL1-UTIL-K7-PCB                
338301                            U2-PBTO-W222-REFL1-UTIL-B6-PCB                
338401                            U2-PBTO-W222-UTUP1-WDK7-PCB                   
338501                            U2-PBTO-W222-UTUP1-WDB6-PCB                   
338601                            U2-PBTO-W222-UTUP1-UTIL-K6-PCB                
338701                            U2-PBTO-W222-UTUP1-UTIL-K7-PCB                
338801                            U2-PBTO-W222-UTUP1-UTIL-B6-PCB                
338901                            U2-REFL2-2501-PCB                             
339001                            U2-REFL2-WDB6-PCB                             
339101                            U2-REFL2-UTIL-WDK6-PCB                        
339201                            U2-REFL2-UTIL-WDK7-PCB                        
339301                            U2-REFL2-UTIL-WDB6-PCB                        
339401                            U2-W222-WDK6-PCB                              
339501                            U2-W222-WDK7-PCB                              
339601                            U2-W222-ARTM-PCB                              
339701                            U2-W222-2501-PCB                              
339801                            U2-W222-WDB6R-PCB                             
339901                            U2-W222-WDK7R-PCB                             
340001                            U2-W222-WDB6-PCB                              
340101                            U2-W222-WDD7-PCB                              
340201                            U2-W222-WDK7E-PCB                             
340301                            U2-W222-UTIL-WDK6-PCB                         
340401                            U2-W222-UTIL-WDK7-PCB                         
340501                            U2-W222-UTIL-WDB6-PCB                         
340601                            U2-W222-UTUP1-WDK7-PCB                        
340701                            U2-W222-UTUP1-WDB6-PCB                        
340801                            U2-W222-UTUP1-UTIL-WDK6-PCB                   
340901                            U2-W222-UTUP1-UTIL-WDK7-PCB                   
341001                            U2-W222-UTUP1-UTIL-WDB6-PCB                   
341101        IF W272-UTUP-KDSVAR-OK                                            
341201           MOVE W272-UTUP-LEADTID-BEHOV TO REFL-IN-LEADTID-BEHOV          
341301        ELSE                                                              
341401           DISPLAY 'W272UTUP-ERROR :' W272-UTUP-TEXT                      
341501           CALL FELLOG                                                    
341601        END-IF                                                            
341701                                                                          
341801        CALL W272REFL USING REFL-W272REFL                                 
341901                            REFL2-2501-PCB                                
342001                            REFL2-WDB6-PCB                                
342101                            REFL2-UTIL-WDK6-PCB                           
342201                            REFL2-UTIL-WDK7-PCB                           
342301                            REFL2-UTIL-WDB6-PCB                           
342401                                                                          
342501        MOVE K6-CREF-TIREFPKT    TO TMP1-YYMMDD                           
342601        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
342701        PERFORM WY2000P1                                                  
342801        IF TMP1-YYMMDD           >= TMP2-YYMMDD                           
342901                                                                          
343001*--- NO UPDATE OF KVREFPKT IF MANUAL DATE IS SET                          
343101          CONTINUE                                                        
343201        ELSE                                                              
343301          MOVE REFL-KVREFPKT     TO K6-CREF-KVREFPKT                      
343401        END-IF                                                            
343501                                                                          
343601        MOVE K6-CREF-TIREFPAF    TO TMP1-YYMMDD                           
343701        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
343801        PERFORM WY2000P1                                                  
343901        IF TMP1-YYMMDD           >= TMP2-YYMMDD                           
344001                                                                          
344101*--- NO UPDATE OF KVREFBER IF MANUAL DATE IS SET                          
344201          CONTINUE                                                        
344301        ELSE                                                              
344401          MOVE REFL-KVREFBER     TO K6-CLAG-KVQ                           
344501        END-IF                                                            
344601                                                                          
344701        MOVE REFL-KVREFOVL       TO K6-CREF-KVREFOVL                      
344801        MOVE REFL-KVSLAGER       TO K6-CLAG-KVSLAGER                      
344901                                                                          
345001        PERFORM S10-GET-FC-TOT                                            
345101                                                                          
345201        IF  K6-CREF-KDREFSTA      = PASSIV                                
345301        AND (K6-CLAG-KVPB-SEP     > ZERO                                  
345401          OR UTIL-KVPB-TOT        > ZERO)                                 
345501          MOVE AKTIV             TO K6-CREF-KDREFSTA                      
345601          MOVE DAGENS-DATUM      TO K6-CREF-TIREFSTA                      
345701        END-IF                                                            
345801        PERFORM IMS-REPL-WDK61129                                         
345901        MOVE JA                  TO SW-HAEMTA-INPUT-FAELT                 
346001     END-IF                                                               
346101     .                                                                    
346201     EJECT                                                                
346301                                                                          
346401 I-KOLLA-INPUT SECTION.                                                   
346501                                                                          
346601     PERFORM IMS-GU-WDK611                                                
346701     IF SEGMENT-FINNS                                                     
346702       MOVE CLAG-VKART     TO WS-VKART                                    
346703       MOVE CLAG-VLARTNTO  TO WS-VLARTNTO                                 
346801       MOVE NEJ              TO SW-KTRL-ERS                               
346901       IF MID-PURCHQTY        = ALL '+'                                   
347001         CONTINUE                                                         
347101       ELSE                                                               
347201         INSPECT MID-PURCHQTY                                             
347301                        REPLACING LEADING SPACE BY ZERO                   
347401         IF MID-PURCHQTY      > ZERO                                      
347501           MOVE JA           TO SW-KTRL-ERS                               
347601         END-IF                                                           
347701       END-IF                                                             
347801*                                                                         
347901       IF SW-KTRL-ERS-JA                                                  
348001        IF CLAG-KDERS         < 10                                        
348101          IF CLAG-PRARTSTD    = ZERO                                      
348201            MOVE PRIS-SAKNAS                                              
348301                             TO MED-IDMFSFEL                              
348401            CALL WMEDKONV USING MED-WMEDAREA                              
348501            MOVE MED-TEMFSFEL                                             
348601                             TO MOD-TEMFSFEL                              
348701            MOVE NEJ         TO INDATA-SW                                 
348801          END-IF                                                          
348901        ELSE                                                              
349001          IF CLAG-KDERS       = +29 OR +52                                
349101            MOVE ARTIKEL-UTGANGEN                                         
349201                             TO MED-IDMFSFEL                              
349301            CALL WMEDKONV USING MED-WMEDAREA                              
349401            MOVE MED-TEMFSFEL                                             
349501                             TO MOD-TEMFSFEL                              
349601          ELSE                                                            
349701            MOVE ARTIKEL-ERSATT                                           
349801                             TO MED-IDMFSFEL                              
349901            CALL WMEDKONV USING MED-WMEDAREA                              
350001            MOVE MED-TEMFSFEL                                             
350101                             TO MOD-TEMFSFEL                              
350201          END-IF                                                          
350301          MOVE NEJ           TO INDATA-SW                                 
350401        END-IF                                                            
350501       END-IF                                                             
350601     ELSE                                                                 
350701       MOVE ARTIKEL-SAKNAS   TO MED-IDMFSFEL                              
350801       CALL WMEDKONV USING MED-WMEDAREA                                   
350901       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
351001       MOVE NEJ              TO INDATA-SW                                 
351101     END-IF                                                               
351201                                                                          
351301     IF INDATA-OK                                                         
351401        PERFORM IA-CHECK-OTH-INPUTS                                       
351501        PERFORM IB-KOLLA-INPUT-NYCKLAR                                    
351601     END-IF                                                               
351701     .                                                                    
351801     EJECT                                                                
351901                                                                          
352001 IA-CHECK-OTH-INPUTS SECTION.                                             
352101                                                                          
352201     IF MID-KVPB-SEP = ALL '+'                                            
352301       MOVE MFS-ADD-LAES-IN-FAELT                                         
352401                             TO MOD-KVPB-SEP-ATTR                         
352501     ELSE                                                                 
352601*      CONVERT FORMAT TO 6 + 1 DECIMAL                                    
352701       MOVE MID-KVPB-SEP     TO DEC-IDFRIDATA                             
352801                                MOD-KVPB-SEP                              
352901       MOVE 6                TO DEC-KVHELTAL                              
353001       MOVE 1                TO DEC-KVDECIMAL                             
353101       CALL WDECEDIT      USING DEC-WDECAREA                              
353201       IF DEC-KDSVAR-OK                                                   
353301         MOVE MFS-ADD-LAES-IN-FAELT                                       
353401                             TO MOD-KVPB-SEP-ATTR                         
353501         MOVE DEC-IDEDITDATA                                              
353601                             TO WS-RED-KVPB-SEP                           
353701                                WS-KVPB-SEP                               
353801         MOVE WS-RED-KVPB-SEP                                             
353901                             TO MOD-KVPB-SEP                              
354001       ELSE                                                               
354101         MOVE MED-3          TO MOD-TEMFSFEL                              
354201         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
354301                             TO MOD-KVPB-SEP-ATTR                         
354401         MOVE NEJ            TO INDATA-SW                                 
354501         MOVE MID-KVPB-SEP   TO MOD-KVPB-SEP                              
354601       END-IF                                                             
354701     END-IF                                                               
354801                                                                          
354901     MOVE CLAG-KVPB-SEP      TO WS-KVPB-SEP-S                             
355001     IF WS-KVPB-SEP       NOT = WS-KVPB-SEP-S                             
355101*    CHECK IF FORECAST CHANGED BY USER                                    
355201        MOVE NEJ             TO SW-HAEMTA-INPUT-FAELT                     
355301        MOVE JA              TO SW-KVPB-SEP                               
355401     ELSE                                                                 
355501*----     IF NO USER INPUT, THEN DISPLAY OF THE FIELD IS                  
355601*----     FROM F-HAEMTA                                                   
355701*----                                                                     
355801        MOVE ZERO            TO WS-KVPB-SEP                               
355901        MOVE NEJ             TO SW-KVPB-SEP                               
356001     END-IF                                                               
356101                                                                          
356301     IF SW-KVPB-SEP-JA                                                    
356401        IF SEGMENT-FINNS                                                  
356501           IF WS-KVPB-SEP     > ZERO                                      
356601              MOVE MFS-ADD-LAES-IN-FAELT                                  
356701                             TO MOD-KVPB-SEP-ATTR                         
356801           END-IF                                                         
356901        ELSE                                                              
357001           CONTINUE                                                       
357101        END-IF                                                            
357201     END-IF                                                               
357301                                                                          
357302     IF SEGMENT-FINNS                                                     
357303        PERFORM IMS-GNP-WDK629                                            
357304        IF SEGMENT-FINNS                                                  
357305           MOVE JA           TO SW-WDK629-STATUS                          
357306        ELSE                                                              
357307           MOVE NEJ          TO SW-WDK629-STATUS                          
357308        END-IF                                                            
357309     END-IF                                                               
357310*                                                                         
357401     IF MID-PURCHQTY          = ALL '+'                                   
357501       MOVE ZERO             TO WS-RED-PURCHQTY                           
357601                                WS-PURCHQTY                               
357701       MOVE WS-RED-PURCHQTY  TO MOD-PURCHQTY                              
357801     ELSE                                                                 
357901       INSPECT MID-PURCHQTY REPLACING LEADING SPACE BY ZERO               
358001       IF MID-PURCHQTY NUMERIC                                            
358101         MOVE MID-PURCHQTY                                                
358201                             TO WS-RED-PURCHQTY                           
358301                                WS-PURCHQTY                               
358401         MOVE WS-RED-PURCHQTY                                             
358501                             TO MOD-PURCHQTY                              
358601                                                                          
358701         IF  MID-PURCHQTY IS NUMERIC                                      
358801         AND MID-PURCHQTY > ZERO                                          
358901           IF (WS-IDREFTYP    = 'A'                                       
359001           OR  WS-IDREFTYP    = 'C'                                       
359101           OR  WS-IDREFTYP    = 'B')                                      
359201             MOVE MFS-ADD-LAES-IN-FAELT                                   
359301                             TO MOD-PURCHQTY-ATTR                         
359401           ELSE                                                           
359501             MOVE MED-8      TO MOD-TEMFSFEL                              
359601             MOVE NEJ        TO INDATA-SW                                 
359701             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
359801                             TO MOD-PURCHQTY-ATTR                         
359901           END-IF                                                         
359902*** CHECK AIR COST                                                        
359903           IF MFS-UPDATE            AND                                   
359904             (WS-IDREFTYP = 'A' OR WS-IDREFTYP = 'C')                     
359905             IF CREF-FLFLYG = 'S'                                         
359906               MOVE MED-18 TO MOD-TEMFSFEL                                
359907               MOVE NEJ  TO INDATA-SW                                     
359908               MOVE MFS-ROER-EJ-FAELT                                     
359909                            TO MOD-PURCHQTY-ATTR                          
359910             ELSE                                                         
359911               PERFORM S11-CALC-AIR-COST                                  
359912               COMPUTE WS-AIR-COST-SEK = WS-AIR-COST-SEK *                
359913                                         WS-PURCHQTY                      
359914               IF WS-AIR-COST-SEK > WS-PRFRAKT                            
359915                  MOVE ERR-HIGH-AIR-COST TO MED-IDMFSFEL                  
359916                  CALL WMEDKONV USING MED-WMEDAREA                        
359917                  MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                      
359918                  MOVE NEJ           TO INDATA-SW                         
359920               END-IF                                                     
359921             END-IF                                                       
359930           END-IF                                                         
359950                                                                          
360001           IF CLAG-KVQPACK-1  > ZERO                                      
360101***      CONTROL TO CHECK IF THE PURCHQTY IS MULTIPLE OF Q1               
360201***                                                                       
360301              DIVIDE MID-PURCHQTY                                         
360401                     BY CLAG-KVQPACK-1                                    
360501                     GIVING WS-SLASK                                      
360601                     REMAINDER WS-REST                                    
360701           END-IF                                                         
360801           IF WS-REST > ZERO                                              
360901*          UNEVEN MULTIPEL OF Q1                                          
361001              MOVE MED-10    TO MOD-TEMFSFEL                              
361101              MOVE NEJ       TO INDATA-SW                                 
361201              MOVE MFS-ADD-LAES-IN-FAELT-HI                               
361301                             TO MOD-PURCHQTY-ATTR                         
361401           END-IF                                                         
361501         ELSE                                                             
361601           MOVE MFS-ADD-LAES-IN-FAELT                                     
361701                             TO MOD-PURCHQTY-ATTR                         
361801         END-IF                                                           
361901       ELSE                                                               
362001         MOVE MED-4          TO MOD-TEMFSFEL                              
362101         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
362201                             TO MOD-PURCHQTY-ATTR                         
362301         MOVE MID-PURCHQTY                                                
362401                             TO MOD-PURCHQTY                              
362501         MOVE NEJ            TO INDATA-SW                                 
362601       END-IF                                                             
362701     END-IF                                                               
362801*                                                                         
363801     IF MID-FLREFBEO          = ALL '+'                                   
363901       MOVE MFS-RENSA-FAELT                                               
364001                             TO MOD-FLREFBEO                              
364101     ELSE                                                                 
364201       MOVE MID-FLREFBEO                                                  
364301                             TO MOD-FLREFBEO                              
364401       IF CLAG-IDDC-REF       > SPACES                                    
364501          IF MID-FLREFBEO     = JA                                        
364601          OR MID-FLREFBEO     = YES                                       
364701          OR MID-FLREFBEO     = NEJ                                       
364801          OR MID-FLREFBEO     = 'S'                                       
364901             MOVE MFS-ADD-LAES-IN-FAELT                                   
365001                             TO MOD-FLREFBEO-ATTR                         
365101          ELSE                                                            
365201             MOVE MED-5      TO MOD-TEMFSFEL                              
365301             MOVE NEJ        TO INDATA-SW                                 
365401             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
365501                             TO MOD-FLREFBEO-ATTR                         
365601          END-IF                                                          
365701       ELSE                                                               
365801*----                                                                     
365901*---- LOKAL ARTIKEL                                                       
366001*----                                                                     
366101          IF MID-FLREFBEO     = NEJ                                       
366201             MOVE MFS-ADD-LAES-IN-FAELT                                   
366301                             TO MOD-FLREFBEO-ATTR                         
366401          ELSE                                                            
366501             MOVE MED-5      TO MOD-TEMFSFEL                              
366601             MOVE NEJ        TO INDATA-SW                                 
366701             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
366801                             TO MOD-FLREFBEO-ATTR                         
366901          END-IF                                                          
367001       END-IF                                                             
367101     END-IF                                                               
367201                                                                          
367301     PERFORM S5-GET-AUTO-REF                                              
367401     IF MID-FLREFBEO      NOT = WS-FLREFBEO-S                             
367501        MOVE NEJ             TO SW-AUTO-REF-SAME                          
367601     END-IF                                                               
367701*                                                                         
367801     IF MID-FLAGGA-FCD        = ALL '+'                                   
367901       IF WS-IDREFTYP         = 'B'                                       
368001         IF CREF-FLFLYG       = 'J'                                       
368101            MOVE MED-11      TO MOD-TEMFSFEL                              
368201            MOVE NEJ         TO INDATA-SW                                 
368301            MOVE MFS-ROER-EJ-FAELT                                        
368401                             TO MOD-FLAGGA-FCD-ATTR                       
368501         END-IF                                                           
368502       ELSE                                                               
368503         IF WS-IDREFTYP       = 'A' OR 'C'                                
368504           IF CREF-FLFLYG     = 'S'                                       
368505              MOVE MED-11    TO MOD-TEMFSFEL                              
368506              MOVE NEJ       TO INDATA-SW                                 
368507              MOVE MFS-ROER-EJ-FAELT                                      
368508                               TO MOD-FLAGGA-FCD-ATTR                     
368601           END-IF                                                         
368602         END-IF                                                           
368603       END-IF                                                             
368701     ELSE                                                                 
368801       MOVE MID-FLAGGA-FCD   TO MOD-FLAGGA-FCD                            
368901       IF MID-FLAGGA-FCD      = 'Y' OR 'J'                                
369001         IF  WS-IDREFTYP      = 'B'                                       
369101         AND MID-PURCHQTY     > ZERO                                      
369201             MOVE NEJ        TO INDATA-SW                                 
369301             MOVE MED-17     TO MOD-TEMFSFEL                              
369401             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
369501                             TO MOD-FLAGGA-FCD-ATTR                       
369801         ELSE                                                             
369901           IF CREF-IDDC-REF   > SPACES                                    
370001              MOVE 'J'       TO WS-FLAGGA-FCD                             
370101              IF MSGI-IDLAND-SPR = 'GB'                                   
370201                 MOVE YES    TO MOD-FLAGGA-FCD                            
370301              ELSE                                                        
370401                 MOVE JA     TO MOD-FLAGGA-FCD                            
370501              END-IF                                                      
370601              MOVE MFS-ADD-LAES-IN-FAELT                                  
370701                             TO MOD-FLAGGA-FCD-ATTR                       
370801           ELSE                                                           
370901              MOVE MED-14    TO MOD-TEMFSFEL                              
371001              MOVE NEJ       TO INDATA-SW                                 
371101              MOVE MFS-ADD-LAES-IN-FAELT-HI                               
371201                             TO MOD-FLAGGA-FCD-ATTR                       
371301           END-IF                                                         
371401         END-IF                                                           
371501       ELSE                                                               
371601         IF MID-FLAGGA-FCD    = 'N'                                       
371701           MOVE 'N'          TO WS-FLAGGA-FCD                             
371801           MOVE WS-FLAGGA-FCD                                             
371901                             TO MOD-FLAGGA-FCD                            
372001           MOVE MFS-ADD-LAES-IN-FAELT                                     
372101                             TO MOD-FLAGGA-FCD-ATTR                       
372102         ELSE                                                             
372103           IF MID-FLAGGA-FCD  = 'S'                                       
372104             MOVE 'S'        TO WS-FLAGGA-FCD                             
372105             MOVE WS-FLAGGA-FCD                                           
372106                               TO MOD-FLAGGA-FCD                          
372107             MOVE MFS-ADD-LAES-IN-FAELT                                   
372108                               TO MOD-FLAGGA-FCD-ATTR                     
372201           ELSE                                                           
372301             MOVE MED-13     TO MOD-TEMFSFEL                              
372401             MOVE NEJ        TO INDATA-SW                                 
372501             MOVE MFS-ADD-LAES-IN-FAELT-HI                                
372601                               TO MOD-FLAGGA-FCD-ATTR                     
372701           END-IF                                                         
372702         END-IF                                                           
372801       END-IF                                                             
372901     END-IF                                                               
373001                                                                          
373101     PERFORM S4-GET-ALW-AIR                                               
373201     IF MID-FLAGGA-FCD    NOT = WS-FLAGGA-FCD-S                           
373301        MOVE NEJ             TO SW-ALW-AIR-SAME                           
373401     END-IF                                                               
373501*                                                                         
373601     IF MID-COMMENT-1         = ALL '+'                                   
373701       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT(1)                            
373801                                MID-COMMENT-1                             
373901     ELSE                                                                 
374001       MOVE MID-COMMENT-1    TO MOD-COMMENT(1)                            
374101     END-IF                                                               
374201     MOVE MFS-ADD-LAES-IN-FAELT                                           
374301                             TO MOD-COMMENT-ATTR(1)                       
374401     IF MID-COMMENT-2         = ALL '+'                                   
374501       MOVE MFS-RENSA-FAELT  TO MOD-COMMENT(2)                            
374601                                MID-COMMENT-2                             
374701     ELSE                                                                 
374801       MOVE MID-COMMENT-2    TO MOD-COMMENT(2)                            
374901     END-IF                                                               
375001     MOVE MFS-ADD-LAES-IN-FAELT                                           
375101                             TO MOD-COMMENT-ATTR(2)                       
375201                                                                          
375301     PERFORM S6-GET-COMMENT                                               
375401     IF   MID-COMMENT-1   NOT = ALL '+'                                   
375501       IF MID-COMMENT-1   NOT = WS-COMMENT-1                              
375601          MOVE NEJ           TO SW-COMMENT-SAME                           
375701       END-IF                                                             
375801     END-IF                                                               
375901     IF   MID-COMMENT-2   NOT = ALL '+'                                   
376001       IF MID-COMMENT-2   NOT = WS-COMMENT-2                              
376101          MOVE NEJ           TO SW-COMMENT-SAME                           
376201       END-IF                                                             
376301     END-IF                                                               
376401     .                                                                    
376501     EJECT                                                                
376601                                                                          
376701 IB-KOLLA-INPUT-NYCKLAR SECTION.                                          
376801                                                                          
376901     IF NOT (MID-IDARTNR-IN       = ALL '+'                               
377001        AND  MID-IDTYPE-IN        = ALL '+'                               
377101        AND  MID-IDPERSON-BUY-IN  = ALL '+'                               
377201        AND  MID-IDSTATUS-IN      = ALL '+')                              
377301                                                                          
377401       MOVE NEJ              TO INDATA-SW                                 
377501     END-IF                                                               
377601     .                                                                    
377701     EJECT                                                                
377801                                                                          
377901 J-FYLL-I-ANT-REVIEW SECTION.                                             
378001                                                                          
378101     MOVE ZERO               TO WS-ANT-REVIEW                             
378201     MOVE WS-IDTYPE          TO W-KDREFTYP-MIN                            
378301                                W-KDREFTYP-MAX                            
378401     MOVE ZERO               TO W-IDARTNR-MIN                             
378501                                                                          
378601     PERFORM IMS-GU-WDE301-MIN-MAX                                        
378701                                                                          
378801     PERFORM UNTIL SEGMENT-SAKNAS                                         
378901                                                                          
379001         IF  REF-KDREFTYP     = WS-IDTYPE                                 
379101         AND REF-IDPERSON-BUY = WS-IDPERSON-BUY-NUM                       
379201         AND REF-KDREFORS     = 'P'                                       
379301         AND REF-IDDC         = IDDC-WS                                   
379401            ADD +1           TO WS-ANT-REVIEW                             
379501            MOVE REF-IDARTNR                                              
379601                             TO WS-SPARA-IDARTNR                          
379701                                                                          
379801            PERFORM UNTIL SEGMENT-SAKNAS                                  
379901            OR WS-SPARA-IDARTNR NOT = REF-IDARTNR                         
380001                                                                          
380101               PERFORM IMS-GN-WDE301-MIN-MAX                              
380201            END-PERFORM                                                   
380301                                                                          
380401         ELSE                                                             
380501            PERFORM IMS-GN-WDE301-MIN-MAX                                 
380601         END-IF                                                           
380701                                                                          
380801     END-PERFORM                                                          
380901     MOVE WS-ANT-REVIEW      TO MOD-ANT-REVIEW                            
381001     .                                                                    
381101     EJECT                                                                
381201                                                                          
381301 K-FYLL-I-NYCKEL-FAELT SECTION.                                           
381401                                                                          
381501     MOVE REF-IDARTNR        TO WS-SPARA-IDARTNR                          
381601                                W-IDARTNR                                 
381701                                MOD-IDARTNR-UT                            
381801     MOVE JA                 TO SW-TRAEFF                                 
381901     MOVE REF-IDDC           TO WS-SPARA-IDDC                             
382001                                W-IDDC                                    
382101     MOVE REF-IDPERSON-BUY TO WS-IDPERSON-BUY-RED                         
382201     MOVE WS-IDPERSON-BUY-RED                                             
382301                             TO MOD-IDPERSON-BUY-UT                       
382401     IF REF-KDREFTYP = 'A'                                                
382501        MOVE 'AIR'           TO MOD-IDTYPE-UT                             
382601                                WS-IDTYPE                                 
382701     END-IF                                                               
382801     IF REF-KDREFTYP = 'C'                                                
382901        MOVE 'AIRCR'         TO MOD-IDTYPE-UT                             
383001        MOVE 'C'             TO WS-IDTYPE                                 
383101     END-IF                                                               
383201     IF REF-KDREFTYP = 'B'                                                
383301        MOVE 'BOAT'          TO MOD-IDTYPE-UT                             
383401                                WS-IDTYPE                                 
383501     END-IF                                                               
383601     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
383701     .                                                                    
383801     EJECT                                                                
383901                                                                          
384001 M-UPD-MOD-FAELT SECTION.                                                 
384101                                                                          
384201     MOVE REF-KDREFTXT       TO WS-REF-KDREFTXT                           
384401*         CODE FOR REFILL WARNINGTEXT                                     
384501     IF REF-KDREFORS = 'P'                                                
384601        MOVE 'PROPOSAL NOT REVIEWED'                                      
384701                             TO MOD-ORDERSTATUS                           
384801        MOVE 'NOT REVIEWED'                                               
384901                             TO MOD-IDSTATUS-UT                           
385001        MOVE 'N'             TO WS-STATUS                                 
385101     ELSE                                                                 
385201        MOVE 'REVIEWED'      TO MOD-ORDERSTATUS                           
385301     END-IF                                                               
385401                                                                          
385501     IF REF-KDREFTYP = 'B'                                                
385601       MOVE 'BOAT '          TO MOD-IDREFTYP-UT                           
385701     END-IF                                                               
385801     IF REF-KDREFTYP = 'A'                                                
385901       MOVE 'AIR  '          TO MOD-IDREFTYP-UT                           
386001     END-IF                                                               
386101     IF REF-KDREFTYP = 'C'                                                
386201       MOVE 'AIRCR'          TO MOD-IDREFTYP-UT                           
386301     END-IF                                                               
386401                                                                          
386501     MOVE REF-KVBEART        TO WS-RED-PURCHQTY                           
386601                                WS-PURCHQTY                               
386701                                WS-PURCHQTY-SIM                           
386801     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY                              
386901     MOVE MFS-ADD-LAES-IN-FAELT                                           
387001                             TO MOD-PURCHQTY-ATTR                         
387101     .                                                                    
387201     EJECT                                                                
387301                                                                          
387401 N-UPD-WDE3-WDK6 SECTION.                                                 
387501                                                                          
387601     MOVE WS-IDREFTYP        TO W-KDREFTYP-MIN                            
387701     MOVE W-IDARTNR          TO W-IDARTNR-MIN                             
387801     MOVE ZERO               TO W-IDDISTR-MIN                             
387901                                                                          
388001     PERFORM IMS-GU-WDK601                                                
388101                                                                          
388201     PERFORM IMS-GU-WDE301-MIN-MAX                                        
388301                                                                          
388401     PERFORM UNTIL SEGMENT-SAKNAS                                         
388501       OR (REF-IDPERSON-BUY   = W-IDPERSON-BUY-MIN                        
388601       AND REF-KDREFTYP       = W-KDREFTYP-MIN                            
388701       AND REF-IDARTNR        = W-IDARTNR-MIN)                            
388801                                                                          
388901       PERFORM IMS-GN-WDE301-MIN-MAX                                      
389001                                                                          
389101     END-PERFORM                                                          
389201                                                                          
389301                                                                          
389401     IF SEGMENT-FINNS                                                     
389501                                                                          
389601       PERFORM IMS-GHU-WDK61129                                           
389701                                                                          
389801       IF REF-KDREFORS        = 'O'                                       
389901         SUBTRACT REF-KVBEART                                             
390001                           FROM K6-CLAG-KVBEART                           
390101       END-IF                                                             
390201                                                                          
390301       IF  MID-PURCHQTY IS NUMERIC                                        
390401       AND MID-PURCHQTY > ZERO                                            
390501         ADD MID-PURCHQTY    TO K6-CLAG-KVBEART                           
390601         MOVE DAGENS-DATUM   TO K6-CREF-TIORDREG                          
390701       END-IF                                                             
390801                                                                          
390901*                                                                         
391001       MOVE REF-IDDC         TO W-IDDC-301                                
391101       MOVE REF-IDPERSON-BUY                                              
391201                             TO W-IDPERSON-BUY                            
391301       MOVE REF-KDREFTYP     TO W-KDREFTYP                                
391401       MOVE REF-IDARTNR      TO W-IDARTNR-301                             
391501       MOVE REF-IDDISTR      TO W-IDDISTR                                 
391502       MOVE REF-IDDC-REF     TO W-IDDC-REF                                
391601                                                                          
391701       PERFORM IMS-GHU-WDE301                                             
391801                                                                          
391901       MOVE WS-IDREFTYP      TO REF-KDREFTYP                              
392001       IF  MID-PURCHQTY IS NUMERIC                                        
392101       AND MID-PURCHQTY > ZERO                                            
392201         MOVE MID-PURCHQTY                                                
392301                             TO REF-KVBEART                               
392401         MOVE 'O'            TO REF-KDREFORS                              
392501         MOVE ART-IDLEVNR    TO REF-IDLEVNR                               
392601         MOVE WS-KDFRAKT     TO REF-KDFRAKT                               
392701         PERFORM IMS-REPL-WDE301                                          
392801       ELSE                                                               
392901         PERFORM IMS-DLET-WDE301                                          
393001* --     IF USER ZEROED PURCHQTY, SET FLREFNYO TO JA                      
393101         IF  K6-CLAG-IDDC-REF > SPACES                                    
393301           IF SEGMENT-FINNS                                               
393401             MOVE JA         TO K6-CREF-FLREFNYO                          
393501           END-IF                                                         
393601         END-IF                                                           
393701       END-IF                                                             
393801       PERFORM IMS-REPL-WDK61129                                          
393901     ELSE                                                                 
394001       IF  MID-PURCHQTY IS NUMERIC                                        
394101       AND MID-PURCHQTY > ZERO                                            
394201                                                                          
394301         MOVE WS-IDREFTYP  TO REF-KDREFTYP                                
394401         MOVE W-IDDC                                                      
394501                           TO REF-IDDC                                    
394601         MOVE W-IDARTNR    TO REF-IDARTNR                                 
394701         PERFORM IMS-GHU-WDK61129                                         
394801         ADD MID-PURCHQTY                                                 
394901                           TO K6-CLAG-KVBEART                             
395001         MOVE DAGENS-DATUM TO K6-CREF-TIORDREG                            
395101         PERFORM IMS-REPL-WDK61129                                        
395201*        --- ACCESS WDK7 TO GET LOCATION INFORMATION                      
395301         MOVE K6-CLAG-IDDC-REF                                            
395401                           TO W-IDDC-REF                                  
395501         PERFORM IMS-GU-WDK711                                            
395601         MOVE SLAG-ADART   TO REF-ADART-CDC                               
395701*                                                                         
395801         MOVE K6-CLAG-ADART                                               
395901                           TO REF-ADART-SDC                               
396001         MOVE WS-IDDISTR   TO REF-IDDISTR                                 
396101         MOVE MID-PURCHQTY                                                
396201                           TO REF-KVBEART                                 
396301         MOVE 'O'          TO REF-KDREFORS                                
396401         MOVE ART-IDLEVNR  TO REF-IDLEVNR                                 
396501         MOVE WS-IDPERSON-BUY-NUM                                         
396601                           TO REF-IDPERSON-BUY                            
396701         MOVE ZERO         TO REF-IDKUNDNR                                
396801                              REF-KDREFTXT                                
396901                              REF-KVBEART-CD                              
397001                              REF-ADLAGOMR-CD                             
397101                              REF-ADGANG-CD                               
397201                              REF-ADPLATS-CD                              
397301         MOVE WS-KDFRAKT   TO REF-KDFRAKT                                 
397302         MOVE CREF-IDDC-REF                                               
397303                           TO REF-IDDC-REF                                
397401         PERFORM IMS-ISRT-WDE301                                          
397501       END-IF                                                             
397601     END-IF                                                               
397701     MOVE 'REVIEWED'         TO MOD-ORDERSTATUS                           
397801     .                                                                    
397901     EJECT                                                                
398001                                                                          
398101 P-CHECK-REFILL-RULE      SECTION.                                        
398201                                                                          
398301     IF  (SW-TRAEFF-JA                                                    
398401     AND  W-IDARTNR           NOT = W-IDARTNR-SAVED)                      
398501     OR  (MFS-NEXT                                                        
398601     AND  W-IDARTNR               > ZERO )                                
398701     OR  MFS-IDTRANS              = '2391'                                
398801        PERFORM S7-CHECK-REFILL-PART-K6                                   
398901        IF NYCKLAR-OK                                                     
399001           PERFORM S8-CHECK-REFILL-PART-K7                                
399101           IF REFILL-ALLOWED-NEJ                                          
399201              MOVE MED-16        TO MOD-TEMFSFEL                          
399301              PERFORM MFS-RENSA-FAELT-IN                                  
399401              PERFORM MFS-RENSA-FAELT-UT                                  
399501              MOVE W-IDARTNR     TO W-IDARTNR-SAVED                       
399601              MOVE ZERO          TO W-IDARTNR                             
399701           END-IF                                                         
399801        END-IF                                                            
399901     END-IF                                                               
400001*                                                                         
400101     IF  SW-TRAEFF-NEJ                                                    
400201     AND W-IDARTNR                = ZERO                                  
400301         MOVE ZERO               TO W-IDARTNR-SAVED                       
400401     END-IF                                                               
400501     .                                                                    
400601     EJECT                                                                
400701                                                                          
400801 S1-SECURITY-CHECK-PARTNO SECTION.                                        
400901     SKIP2                                                                
401001*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
401101     PERFORM IMS-GU-WDK601                                                
401201     IF  SEGMENT-FINNS                                                    
401301       MOVE ART-IDLEVNR          TO WS-IDLEVNR-8                          
401401       IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                          
401501       OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                    
401601*        --- USER GRANTED                                                 
401701         SET PASSED-SECURITY-CHECK TO TRUE                                
401801       ELSE                                                               
401901         SET BLOCKED-SECURITY-CHECK TO TRUE                               
402001       END-IF                                                             
402101     END-IF                                                               
402201     .                                                                    
402301     EJECT                                                                
402401                                                                          
402501 S2-LAES-WDA5A-KL1-EJ-REF SECTION.                                        
402601                                                                          
402701     MOVE W-IDARTNR          TO W-IDARTNR-N3-MIN                          
402801                                W-IDARTNR-N3-MAX                          
402901     MOVE WS-IDDC-CDC        TO W-IDDC-N3-MIN                             
403001                                W-IDDC-N3-MAX                             
403101                                                                          
403201     PERFORM IMS-GN-WDA5A1-KL1                                            
403301                                                                          
403401     MOVE ZERO               TO WS-RESTKVANT                              
403501     PERFORM UNTIL SEGMENT-SAKNAS                                         
403601*  SUM RESTKVANT                                                          
403701        MOVE SEQA-IDDISTR    TO DIST35-IDDISTR                            
403801        IF NOT DIST35-NONVCC-CDC-REFILL                                   
403901           ADD SEQA-KVART    TO WS-RESTKVANT                              
404001        END-IF                                                            
404101        PERFORM IMS-GN-WDA5A1-KL1                                         
404201     END-PERFORM                                                          
404301     .                                                                    
404401     EJECT                                                                
404501                                                                          
404601 S3-CALC-VV-I-AA SECTION.                                                 
404701                                                                          
404801*    --- CALCULATE THE YEAR HAD HOW MANY WEEKS                            
404901     MOVE TIAA               TO WS-AAR                                    
405001     MOVE 53                 TO WS-VV                                     
405101     MOVE 'AAVV  '           TO DAT-KDDATFORM                             
405201     MOVE TIAAVV             TO DAT-I-TIDATUM                             
405301     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
405401                         DAT-O-TIDATUM DAT-KDSVAR                         
405501     IF DAT-KDSVAR-OK                                                     
405601       MOVE 53               TO WS-ANT-VV                                 
405701     ELSE                                                                 
405801       MOVE 52               TO WS-ANT-VV                                 
405901     END-IF                                                               
406001     .                                                                    
406101     EJECT                                                                
406201                                                                          
406301 S4-GET-ALW-AIR     SECTION.                                              
406401                                                                          
406501     IF   CREF-FLFLYG         = JA                                        
406601     AND  MSGI-IDLAND-SPR     = 'GB'                                      
406701          MOVE YES           TO WS-FLAGGA-FCD-S                           
406801     ELSE                                                                 
406901          MOVE CREF-FLFLYG   TO WS-FLAGGA-FCD-S                           
407001     END-IF                                                               
407101     .                                                                    
407201     EJECT                                                                
407301                                                                          
407401 S5-GET-AUTO-REF    SECTION.                                              
407501                                                                          
407601     IF  CREF-FLREFBEO        = JA                                        
407701     AND MSGI-IDLAND-SPR      = 'GB'                                      
407801         MOVE YES            TO WS-FLREFBEO-S                             
407901     ELSE                                                                 
408001         MOVE CREF-FLREFBEO                                               
408101                             TO WS-FLREFBEO-S                             
408201     END-IF                                                               
408301     .                                                                    
408401     EJECT                                                                
408501                                                                          
408601 S6-GET-COMMENT     SECTION.                                              
408701                                                                          
408801     MOVE 1                  TO W-KDNOTTYP                                
408901     PERFORM IMS-GU-WDK625                                                
409001     IF SEGMENT-FINNS                                                     
409101        MOVE NOT-TEARTNOT (1:36)                                          
409201                             TO WS-COMMENT-1                              
409301     ELSE                                                                 
409401        MOVE SPACES          TO WS-COMMENT-1                              
409501     END-IF                                                               
409601*                                                                         
409701     MOVE 2                  TO W-KDNOTTYP                                
409801     PERFORM IMS-GU-WDK625                                                
409901     IF SEGMENT-FINNS                                                     
410001        MOVE NOT-TEARTNOT (1:36)                                          
410101                             TO WS-COMMENT-2                              
410201     ELSE                                                                 
410301        MOVE SPACES          TO WS-COMMENT-2                              
410401     END-IF                                                               
410501     .                                                                    
410601     EJECT                                                                
410701                                                                          
410801 S7-CHECK-REFILL-PART-K6 SECTION.                                         
410901                                                                          
411001     PERFORM IMS-GU-WDK611                                                
411101     IF SEGMENT-FINNS                                                     
411201        IF CLAG-IDDC-REF = SPACE                                          
411301           SET NOT-REFILL-PART  TO TRUE                                   
411401           MOVE NEJ             TO NYCKLAR-SW                             
411501                                   INDATA-SW                              
411601        ELSE                                                              
411701           SET REFILL-PART      TO TRUE                                   
411801           MOVE CLAG-IDDC-REF   TO W-IDDC-B616                            
411901                                   WS-IDDC-REF                            
412001           PERFORM IMS-GU-WDB616                                          
412101           IF SEGMENT-FINNS                                               
412201              MOVE B6-REF-IDDISTR-REFILL                                  
412301                                TO WS-IDDISTR                             
412401              MOVE B6-REF-REAIRCO                                         
412501                                TO WS-REAIRCO                             
412502              MOVE B6-REF-PRFRAKT                                         
412503                                TO WS-PRFRAKT                             
412601           ELSE                                                           
412701              MOVE NEJ          TO NYCKLAR-SW                             
412801                                   INDATA-SW                              
412900              SET DISTRICT-NOT-FOUND TO TRUE                              
413000           END-IF                                                         
413100        END-IF                                                            
413200     END-IF                                                               
413300*                                                                         
413400     IF NYCKLAR-FEL                                                       
413500       IF NOT-REFILL-PART                                                 
413600          MOVE ERR-NOT-REFILL-PART                                        
413700                                 TO MED-IDMFSFEL                          
413800       END-IF                                                             
413900       IF DISTRICT-NOT-FOUND                                              
414000          MOVE MED-15            TO MOD-TEMFSFEL                          
414100       ELSE                                                               
414200          CALL WMEDKONV       USING MED-WMEDAREA                          
414300          MOVE MED-TEMFSFEL      TO MOD-TEMFSFEL                          
414400       END-IF                                                             
414500       PERFORM MFS-RENSA-FAELT-IN                                         
414600       PERFORM MFS-RENSA-FAELT-UT                                         
414700       MOVE W-IDARTNR            TO W-IDARTNR-SAVED                       
414800       MOVE ZERO                 TO W-IDARTNR                             
414900     END-IF                                                               
415000     .                                                                    
415100     EJECT                                                                
415200                                                                          
415300 S8-CHECK-REFILL-PART-K7 SECTION.                                         
415400                                                                          
415500***   CHECK IF SUPPLIER FOR REFILLING DC EQUALS 1441 OR IF                
415600***   NONE OF REFILLING DC IS REFILLED FROM OTHER MARKET                  
415700***   IF TRUE THE REFILL OF THE PART FROM NDC TO CDC                      
415800***   IS NOT POSSIBLE.                                                    
415900     MOVE CLAG-IDDC-REF          TO W-IDDC-REF                            
416000     PERFORM IMS-GU-WDK711                                                
416100     IF SEGMENT-FINNS                                                     
416200        MOVE SLAG-IDDC           TO WS-IDDC                               
416300        MOVE SLAG-IDDC-REF       TO REF-WS-IDDC                           
416400        IF REF-CDC-SE                                                     
416500           SET REFILL-ALLOWED-NEJ  TO TRUE                                
416600           MOVE NEJ                TO NYCKLAR-SW                          
416700                                      INDATA-SW                           
416800        ELSE                                                              
417100          IF  NDC                                                         
417200          AND REF-NDC                                                     
417300            SET REFILL-ALLOWED-NEJ TO TRUE                                
417400            MOVE NEJ               TO NYCKLAR-SW                          
417500                                       INDATA-SW                          
417600          END-IF                                                          
417700        END-IF                                                            
417800     ELSE                                                                 
417900        SET REFILL-ALLOWED-NEJ   TO TRUE                                  
418000        MOVE NEJ                 TO NYCKLAR-SW                            
418100                                    INDATA-SW                             
418200     END-IF                                                               
418300     .                                                                    
418400     EJECT                                                                
418500                                                                          
418600 S9-SEARCH-IDLAND SECTION.                                                
418700                                                                          
418800     SEARCH ALL DC-LAND                                                   
418900       AT END                                                             
419000         MOVE SPACE          TO W-IDLAND                                  
419100       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC-REF                         
419200         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
419300     END-SEARCH                                                           
419400     .                                                                    
419500     EJECT                                                                
419600                                                                          
419700 S10-GET-FC-TOT       SECTION.                                            
419800                                                                          
419900     INITIALIZE UTIL-W271UTIL                                             
420000     MOVE W-IDARTNR                 TO UTIL-IDARTNR                       
420100     MOVE 003                       TO UTIL-KDCALL                        
420200                                                                          
420300     CALL W271UTIL USING UTIL-W271UTIL                                    
420400                         UTIL-WDK6-PCB                                    
420500                         UTIL-WDK7-PCB                                    
420600                         UTIL-WDB6-PCB                                    
420700                                                                          
420800     IF UTIL-KDSVAR-OK                                                    
420900        MOVE UTIL-KVPB-TOT      TO MOD-KVPB-TOT                           
421000     ELSE                                                                 
421100        STRING 'FEL FRÅN W271UTIL '   UTIL-KDSVAR                         
421200        DELIMITED BY SIZE INTO FELTEXT                                    
421300        CALL FELLOG                                                       
421400     END-IF                                                               
421500     .                                                                    
421600     EJECT                                                                
421700                                                                          
421710 S11-CALC-AIR-COST    SECTION.                                            
421720                                                                          
421730     MOVE ZERO               TO WS-KR-VIKT                                
421740                                WS-KR-VIKT-RED                            
421750                                WS-KR-VOLYM                               
421760                                WS-KR-VOLYM-RED                           
421770                                WS-AIR-COST-SEK                           
421780                                                                          
421790                                                                          
421791     COMPUTE WS-KR-VIKT ROUNDED                                           
421792                              = (WS-VKART * WS-REAIRCO ) / 1000           
421793     COMPUTE WS-KR-VIKT-RED ROUNDED                                       
421794                              = WS-KR-VIKT * 1                            
421795     COMPUTE WS-KR-VOLYM ROUNDED                                          
421796                   = (WS-VLARTNTO * WS-REAIRCO * 167) / 1000000           
421797     COMPUTE WS-KR-VOLYM-RED ROUNDED                                      
421798                              = WS-KR-VOLYM * 1                           
421799*                                                                         
421800*     ---- DISPLAY THE MAXIMUM COST                                       
421801*                                                                         
421802     IF WS-KR-VIKT-RED        > WS-KR-VOLYM-RED                           
421803       MOVE WS-KR-VIKT-RED   TO MOD-AIR-COST-SEK                          
421804                                WS-AIR-COST-SEK                           
421805     ELSE                                                                 
421806       MOVE WS-KR-VOLYM-RED  TO MOD-AIR-COST-SEK                          
421807                                WS-AIR-COST-SEK                           
421808     END-IF                                                               
421809     .                                                                    
421810     EJECT                                                                
421811                                                                          
415300 S12-LAES-WDA5-ENTER SECTION.                                             
415700                                                                          
402701     MOVE W-IDARTNR          TO W-IDARTNR-N4-MIN                          
402801                                W-IDARTNR-N4-MAX                          
                                                                                
415800     PERFORM IMS-GN-WDA5A1-KL2                                            
415900                                                                          
416000     MOVE ZERO               TO WS-RESTKVANT                              
416100     PERFORM UNTIL SEGMENT-SAKNAS                                         
416200                                                                          
416300        MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                             
416400        MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                            
416500        MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                            
416600        MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                             
416700        MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                             
416800                                                                          
403701        MOVE SEQA-IDDISTR    TO DIST35-IDDISTR                            
403801        IF DIST35-NONVCC-CDC-REFILL                                       
417000            PERFORM IMS-GU-WDA501                                         
417100            IF SEGMENT-FINNS                                              
417200            AND RAD-KDSTARAD = '2'                                        
417400*  SUM BACKORDER QUANTITY                                                 
417500            ADD RAD-KVRO   TO WS-RESTKVANT                                
417600            END-IF                                                        
417700        END-IF                                                            
417800                                                                          
417900        PERFORM IMS-GN-WDA5A1-KL2                                         
418000*                                                                         
418100     END-PERFORM                                                          
418200     .                                                                    
418300     EJECT                                                                
418400                                                                          
421820 MFS-RENSA-FAELT-IN SECTION.                                              
421900                                                                          
422000*    --- ALL INPUT FIELDS                                                 
422100     MOVE MFS-RENSA-FAELT    TO MOD-KVPB-SEP                              
422200                                MOD-PURCHQTY                              
422300                                MOD-FLREFBEO                              
422400                                MOD-FLAGGA-FCD                            
422500     MOVE 1                  TO IX                                        
422600     PERFORM UNTIL IX > 2                                                 
422700       MOVE MFS-RENSA-FAELT  TO                                           
422800                                MOD-COMMENT(IX)                           
422900       ADD 1                 TO IX                                        
423000     END-PERFORM                                                          
423100     MOVE ZERO               TO WS-RED-PURCHQTY                           
423200     MOVE WS-RED-PURCHQTY    TO MOD-PURCHQTY                              
423300     .                                                                    
423400     EJECT                                                                
423500                                                                          
423600 MFS-RENSA-FAELT-UT SECTION.                                              
423700                                                                          
423800*    --- ALL OUTPUT FIELDS                                                
423900     MOVE MFS-RENSA-FAELT    TO MOD-BEART                                 
424000                                MOD-ANT-REVIEW                            
424100                                MOD-FOREG-AAR                             
424200                                MOD-IAAR                                  
424300                                MOD-KVOI-IAAR                             
424400                                MOD-KVOI-FOREG-AAR                        
424500                                MOD-VECKA                                 
424600                                MOD-KVOI-INNEV                            
424700                                MOD-FLWILSON                              
424800                                MOD-VKART                                 
424900                                MOD-VLARTNTO                              
425000                                MOD-TIREFEFT                              
425100                                MOD-TIINLINL                              
425200                                MOD-TIORDREG                              
425300                                MOD-ADLAGOMR                              
425400                                MOD-ADGANG                                
425500                                MOD-ADPLATS                               
425600                                MOD-KVREFPKT                              
425700                                MOD-KVREFBER                              
425800                                MOD-BALANCE                               
425900                                MOD-KVAKS-CDC                             
426000                                MOD-ORDERED                               
426100                                MOD-KVROS                                 
426200                                MOD-CRIT-KVROS-NDC-CDC                    
426300                                MOD-SUPERWEEK                             
426400                                MOD-QUALBLOCK                             
426500                                MOD-IDFKNGRP                              
426600                                MOD-KDPRODSL                              
426700                                MOD-REPLACES                              
426800                                MOD-TIFINLV                               
426900                                MOD-TIURPROD                              
427000                                MOD-REPL-BY                               
427100                                MOD-KDERS                                 
427200                                MOD-KVQPACK-0                             
427300                                MOD-PRARTSTD                              
427400                                MOD-KVQPACK-1                             
427500                                MOD-AIR-COST-SEK                          
427600                                MOD-KVQPACK-3                             
427700                                MOD-KVQPACK-4                             
427800                                MOD-KVAVIS                                
427900                                MOD-DAPUBL                                
428000                                MOD-KVPALL                                
428100                                MOD-AVAIL                                 
428200                                MOD-KVAKS-SDC                             
428300                                MOD-KVROS-SDC                             
428400                                MOD-SEASON                                
428500                                MOD-KVPB-SDC                              
428600                                MOD-KVPB-TOT                              
428700                                MOD-KVROS-NDC-CDC                         
428800                                MOD-FLREFBEO                              
428900                                MOD-FLAGGA-FCD                            
429000                                MOD-REDIRLEV                              
429100                                                                          
429200     MOVE 1                  TO IX                                        
429300     PERFORM UNTIL IX > 3                                                 
429400       MOVE MFS-RENSA-FAELT  TO MOD-MODEL (IX)                            
429500                                                                          
429600       ADD 1                 TO IX                                        
429700     END-PERFORM                                                          
429800                                                                          
429900     MOVE 1                  TO IX                                        
430000     PERFORM UNTIL IX > 12                                                
430100       MOVE MFS-RENSA-FAELT  TO MOD-TIPP (IX)                             
430200                                MOD-TIVV-FOM-TOM (IX)                     
430300                                MOD-KVOI-RULL (IX)                        
430400       ADD 1                 TO IX                                        
430500     END-PERFORM                                                          
430600     .                                                                    
430700     SKIP3                                                                
430800                                                                          
430900 MFS-LAES-IN-IGEN SECTION.                                                
431000                                                                          
431100*    --- ALL INPUT FIELD                                                  
431200                                                                          
431300     MOVE MFS-ADD-LAES-IN-FAELT                                           
431400                             TO MOD-IDARTNR-IN-ATTR                       
431500                                MOD-KVPB-SEP-ATTR                         
431600                                MOD-PURCHQTY-ATTR                         
431700                                MOD-FLREFBEO-ATTR                         
431800                                MOD-FLAGGA-FCD-ATTR                       
431900     MOVE 1                  TO IX                                        
432000     PERFORM UNTIL IX > 2                                                 
432100       MOVE MFS-ADD-LAES-IN-FAELT                                         
432200                             TO MOD-COMMENT-ATTR(IX)                      
432300       ADD 1                 TO IX                                        
432400     END-PERFORM                                                          
432500     .                                                                    
432600     EJECT                                                                
432700* --- IMS SECTION   ---                                                   
432800     SKIP3                                                                
432900                                                                          
433000 IMS-GET-MSG SECTION.                                                     
433100                                                                          
433200     MOVE '  QC' TO GODK-STATUSKODER                                      
433300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
433400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
433500     PERFORM IMS-STATUSKONTROLL                                           
433600     .                                                                    
433700     SKIP3                                                                
433800 IMS-INSERT-MSG SECTION.                                                  
433900                                                                          
434000     IF ENGLISH-TEXT                                                      
434100       MOVE 'N' TO MFS-KDHUVOMR                                           
434200     END-IF                                                               
434300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
434400     MOVE SPACE TO GODK-STATUSKODER                                       
434500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
434600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
434700     PERFORM IMS-STATUSKONTROLL                                           
434800     .                                                                    
434900     SKIP3                                                                
435000 IMS-GU-WDK601      SECTION.                                              
435100                                                                          
435200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
435300          DELIMITED BY SIZE INTO SSA1                                     
435400     MOVE '  GE' TO GODK-STATUSKODER                                      
435500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
435600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
435700     PERFORM IMS-STATUSKONTROLL                                           
435800     .                                                                    
435900     SKIP3                                                                
436000 IMS-GU-WDK611      SECTION.                                              
436100                                                                          
436200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
436300          DELIMITED BY SIZE INTO SSA1                                     
436400     MOVE 'WDK611  '       TO SSA2                                        
436500     MOVE '  GE' TO GODK-STATUSKODER                                      
436600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
436700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
436800     PERFORM IMS-STATUSKONTROLL                                           
436900     .                                                                    
437000     SKIP3                                                                
437100 IMS-GU-WDK626      SECTION.                                              
437200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
437300          DELIMITED BY SIZE INTO SSA1                                     
437400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
437500          DELIMITED BY SIZE INTO SSA2                                     
437600     MOVE 'WDK626   ' TO SSA3                                             
437700     MOVE '  GE' TO GODK-STATUSKODER                                      
437800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK626                    
437900                      SSA1 SSA2 SSA3                                      
438000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
438100     PERFORM IMS-STATUSKONTROLL                                           
438200     .                                                                    
438300     EJECT                                                                
438400 IMS-GNP-WDK629     SECTION.                                              
438500                                                                          
438600     MOVE 'WDK629  '       TO SSA1                                        
438700     MOVE '  GE' TO GODK-STATUSKODER                                      
438800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1              
438900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
439000     PERFORM IMS-STATUSKONTROLL                                           
439100     .                                                                    
439200     SKIP3                                                                
439300 IMS-GU-WDK629      SECTION.                                              
439400                                                                          
439500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
439600          DELIMITED BY SIZE INTO SSA1                                     
439700     MOVE 'WDK611  '       TO SSA2                                        
439800     MOVE 'WDK629  '       TO SSA3                                        
439900     MOVE '  GE' TO GODK-STATUSKODER                                      
440000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK629                    
440100                                    SSA1 SSA2 SSA3                        
440200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
440300     PERFORM IMS-STATUSKONTROLL                                           
440400     .                                                                    
440500     SKIP3                                                                
440600 IMS-GHU-WDK61129   SECTION.                                              
440700                                                                          
440800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
440900          DELIMITED BY SIZE INTO SSA1                                     
441000     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
441100          DELIMITED BY SIZE INTO SSA2                                     
441200     MOVE 'WDK629  '       TO SSA3                                        
441300     MOVE '  ' TO GODK-STATUSKODER                                        
441400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK61129                 
441500                            SSA1 SSA2 SSA3                                
441600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
441700     PERFORM IMS-STATUSKONTROLL                                           
441800     .                                                                    
441900     SKIP3                                                                
442000 IMS-REPL-WDK61129  SECTION.                                              
442100                                                                          
442200     MOVE '  ' TO GODK-STATUSKODER                                        
442300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK61129                
442400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
442500     PERFORM IMS-STATUSKONTROLL                                           
442600     .                                                                    
442700     SKIP3                                                                
442800 IMS-GU-WDK625      SECTION.                                              
442900                                                                          
443000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
443100          DELIMITED BY SIZE INTO SSA1                                     
443200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
443300          DELIMITED BY SIZE INTO SSA2                                     
443400     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
443500          DELIMITED BY SIZE INTO SSA3                                     
443600     MOVE '  GE' TO GODK-STATUSKODER                                      
443700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK625                    
443800                           SSA1 SSA2 SSA3                                 
443900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
444000     PERFORM IMS-STATUSKONTROLL                                           
444100     .                                                                    
444200     SKIP3                                                                
444300 IMS-GHU-WDK625     SECTION.                                              
444400                                                                          
444500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
444600          DELIMITED BY SIZE INTO SSA1                                     
444700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
444800          DELIMITED BY SIZE INTO SSA2                                     
444900     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
445000          DELIMITED BY SIZE INTO SSA3                                     
445100     MOVE '  GE' TO GODK-STATUSKODER                                      
445200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK625                   
445300                           SSA1 SSA2 SSA3                                 
445400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
445500     PERFORM IMS-STATUSKONTROLL                                           
445600     .                                                                    
445700     SKIP3                                                                
445800 IMS-REPL-WDK625     SECTION.                                             
445900                                                                          
446000     MOVE '  ' TO GODK-STATUSKODER                                        
446100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK625                  
446200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
446300     PERFORM IMS-STATUSKONTROLL                                           
446400     .                                                                    
446500     SKIP3                                                                
446600 IMS-ISRT-WDK625     SECTION.                                             
446700                                                                          
446800     MOVE 'WDK625   ' TO SSA1                                             
446900     MOVE '  ' TO GODK-STATUSKODER                                        
447000     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-AREA-WDK625 SSA1             
447100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
447200     PERFORM IMS-STATUSKONTROLL                                           
447300     .                                                                    
447400     SKIP3                                                                
447500 IMS-DLET-WDK625     SECTION.                                             
447600                                                                          
447700     MOVE '  ' TO GODK-STATUSKODER                                        
447800     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-AREA-WDK625                  
447900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
448000     PERFORM IMS-STATUSKONTROLL                                           
448100     .                                                                    
448200     SKIP3                                                                
448300 IMS-GU-WDK711      SECTION.                                              
448400                                                                          
448500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
448600          DELIMITED BY SIZE INTO SSA1                                     
448700     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
448800          DELIMITED BY SIZE INTO SSA2                                     
448900     MOVE '  GE' TO GODK-STATUSKODER                                      
449000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1   SSA2        
449100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
449200     PERFORM IMS-STATUSKONTROLL                                           
449300     .                                                                    
449400     SKIP3                                                                
449500 IMS-GN-WDK711      SECTION.                                              
449600                                                                          
449700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
449800          DELIMITED BY SIZE INTO SSA1                                     
449900     MOVE 'WDK711  '       TO SSA2                                        
450000     MOVE '  GE' TO GODK-STATUSKODER                                      
450100     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
450200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
450300     PERFORM IMS-STATUSKONTROLL                                           
450400     .                                                                    
450500     EJECT                                                                
450600 IMS-GU-WDK712      SECTION.                                              
450700                                                                          
450800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
450900          DELIMITED BY SIZE INTO SSA1                                     
451000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
451100          DELIMITED BY SIZE INTO SSA2                                     
451200     MOVE '  GE' TO GODK-STATUSKODER                                      
451300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
451400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
451500     PERFORM IMS-STATUSKONTROLL                                           
451600     .                                                                    
451700     SKIP3                                                                
451800 IMS-GU-WDK722      SECTION.                                              
451900                                                                          
452000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
452100          DELIMITED BY SIZE INTO SSA1                                     
452200     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
452300          DELIMITED BY SIZE INTO SSA2                                     
452400     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
452500          DELIMITED BY SIZE INTO SSA3                                     
452600     MOVE '  GE' TO GODK-STATUSKODER                                      
452700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK722                    
452800                                    SSA1 SSA2 SSA3                        
452900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
453000     PERFORM IMS-STATUSKONTROLL                                           
453100     .                                                                    
453200     SKIP3                                                                
453300 IMS-GU-WDD301-BSEQ SECTION.                                              
453400                                                                          
453500     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
453600          DELIMITED BY SIZE INTO SSA1                                     
453700     MOVE '  GE' TO GODK-STATUSKODER                                      
453800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
453900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
454000     PERFORM IMS-STATUSKONTROLL                                           
454100     .                                                                    
454200     SKIP3                                                                
454300 IMS-GNP-WDD311     SECTION.                                              
454400                                                                          
454500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
454600          DELIMITED BY SIZE INTO SSA1                                     
454700     MOVE '  GE' TO GODK-STATUSKODER                                      
454800     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
454900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
455000     PERFORM IMS-STATUSKONTROLL                                           
455100     .                                                                    
455200     SKIP3                                                                
455300 IMS-GHU-WDE301     SECTION.                                              
455400                                                                          
455500     STRING 'WDE301  (WDE301KY =' W-WDE301KY-X                            
455510                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
455600          DELIMITED BY SIZE INTO SSA1                                     
455700     MOVE '  ' TO GODK-STATUSKODER                                        
455800     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-AREA-WDE301 SSA1              
455900     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUSKONTROLL                                           
456100     .                                                                    
456200     SKIP3                                                                
456300 IMS-GU-WDE301-MIN-MAX SECTION.                                           
456400                                                                          
456610     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
456620                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
456630                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
456640                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
456700          DELIMITED BY SIZE INTO SSA1                                     
456800     MOVE '  GE' TO GODK-STATUSKODER                                      
456900     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
457000     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
457100     PERFORM IMS-STATUSKONTROLL                                           
457200     .                                                                    
457300     SKIP3                                                                
457400 IMS-GN-WDE301-MIN-MAX SECTION.                                           
457500                                                                          
457600     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
457700                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
457710                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
457720                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
457800          DELIMITED BY SIZE INTO SSA1                                     
457900     MOVE '  GE' TO GODK-STATUSKODER                                      
458000     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
458100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
458200     PERFORM IMS-STATUSKONTROLL                                           
458300     .                                                                    
458400     SKIP3                                                                
458500 IMS-GU-WDE301-PF8  SECTION.                                              
458600                                                                          
458610     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
458620                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
458630                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
458640                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
458900          DELIMITED BY SIZE INTO SSA1                                     
459000     MOVE '  GE' TO GODK-STATUSKODER                                      
459100     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
459200     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
459300     PERFORM IMS-STATUSKONTROLL                                           
459400     .                                                                    
459500     SKIP3                                                                
459600 IMS-GN-WDE301-PF8  SECTION.                                              
459700                                                                          
459710     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
459720                    '&WDE301KY<=' W-WDE301KY-MAX-X                        
459730                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
459740                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
460000          DELIMITED BY SIZE INTO SSA1                                     
460100     MOVE '  GE' TO GODK-STATUSKODER                                      
460200     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-AREA-WDE301 SSA1               
460300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
460400     PERFORM IMS-STATUSKONTROLL                                           
460500     .                                                                    
460600     SKIP3                                                                
460700 IMS-ISRT-WDE301    SECTION.                                              
460800                                                                          
460900     MOVE 'WDE301   ' TO SSA1                                             
461000     MOVE '  ' TO GODK-STATUSKODER                                        
461100     CALL CBLTDLI USING ISRT WDE3-PCB DLI-IO-AREA-WDE301 SSA1             
461200     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
461300     PERFORM IMS-STATUSKONTROLL                                           
461400     .                                                                    
461500     SKIP3                                                                
461600 IMS-REPL-WDE301    SECTION.                                              
461700                                                                          
461800     MOVE '  ' TO GODK-STATUSKODER                                        
461900     CALL CBLTDLI USING REPL WDE3-PCB DLI-IO-AREA-WDE301                  
462000     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
462100     PERFORM IMS-STATUSKONTROLL                                           
462200     .                                                                    
462300     SKIP3                                                                
462400 IMS-DLET-WDE301    SECTION.                                              
462500                                                                          
462600     MOVE '  ' TO GODK-STATUSKODER                                        
462700     CALL CBLTDLI USING DLET WDE3-PCB DLI-IO-AREA-WDE301                  
462800     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
462900     PERFORM IMS-STATUSKONTROLL                                           
463000     .                                                                    
463100     SKIP3                                                                
463200 IMS-GN-WDA5A1-KL1 SECTION.                                               
463300                                                                          
463400     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A1KY-MIN                          
463500                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
463600                    '&KDORDKL  =' W-KDORDKL-1-X                           
463700                    '&KDSTARAD =' W-KDSTARAD-2-X ')'                      
463800            DELIMITED BY SIZE INTO SSA1                                   
463900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
464000     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-AREA-WDA5A1 SSA1              
464100     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
464200     PERFORM IMS-STATUSKONTROLL                                           
464300     .                                                                    
464400     SKIP3                                                                
470500 IMS-GN-WDA5A1-KL2  SECTION.                                              
470700     STRING 'WDA5A1  (WDA5A1KY>=' W-WDA5A2KY-MIN                          
470800                    '&WDA5A1KY<=' W-WDA5A2KY-MAX  ')'                     
470900            DELIMITED BY SIZE INTO SSA1                                   
471000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
471100     CALL CBLTDLI USING GN WDA5A-PCB DLI-IO-AREA-WDA5A1 SSA1              
471200     MOVE WDA5A-STATUS-CODE TO STATUS-WS                                  
471300     PERFORM IMS-STATUSKONTROLL                                           
471400     .                                                                    
471500     SKIP3                                                                
464500 IMS-GU-WDL601      SECTION.                                              
464600                                                                          
464700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
464800          DELIMITED BY SIZE INTO SSA1                                     
464900     MOVE '  GE' TO GODK-STATUSKODER                                      
465000     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
465100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
465200     PERFORM IMS-STATUSKONTROLL                                           
465300     .                                                                    
465400     SKIP3                                                                
465500 IMS-GNP-WDL611     SECTION.                                              
465600                                                                          
465700     STRING 'WDL611     '                                                 
465800          DELIMITED BY SIZE INTO SSA1                                     
465900     MOVE '  GE' TO GODK-STATUSKODER                                      
466000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
466100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
466200     PERFORM IMS-STATUSKONTROLL                                           
466300     .                                                                    
466400     SKIP3                                                                
466500 IMS-GU-WDN601      SECTION.                                              
466600                                                                          
466700     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
466800            DELIMITED BY SIZE INTO SSA1                                   
466900     MOVE '  GE' TO GODK-STATUSKODER                                      
467000     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-AREA-WDN601 SSA1               
467100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
467200     PERFORM IMS-STATUSKONTROLL                                           
467300     .                                                                    
467400     SKIP3                                                                
467500 IMS-GNP-WDN611     SECTION.                                              
467600                                                                          
467700     MOVE 'WDN611   ' TO SSA1                                             
467800     MOVE '  GE' TO GODK-STATUSKODER                                      
467900     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-AREA-WDN611 SSA1              
468000     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
468100     PERFORM IMS-STATUSKONTROLL                                           
468200     .                                                                    
468300     SKIP3                                                                
468400 IMS-GU-WDD701      SECTION.                                              
468500                                                                          
468600     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
468700            DELIMITED BY SIZE INTO SSA1                                   
468800     MOVE '  GE' TO GODK-STATUSKODER                                      
468900     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-AREA-WDD701 SSA1               
469000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
469100     PERFORM IMS-STATUSKONTROLL                                           
469200     .                                                                    
469300     SKIP3                                                                
469400 IMS-GNP-WDD702     SECTION.                                              
469500                                                                          
469600     STRING 'WDD702  (FLTEXT   =N)'                                       
469700            DELIMITED BY SIZE INTO SSA1                                   
469800     MOVE '  GE' TO GODK-STATUSKODER                                      
469900     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD702 SSA1              
470000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
470100     PERFORM IMS-STATUSKONTROLL                                           
470200     .                                                                    
470300     SKIP3                                                                
470400 IMS-GNP-WDD704      SECTION.                                             
470500                                                                          
470600     STRING 'WDD704     '                                                 
470700            DELIMITED BY SIZE INTO SSA1                                   
470800     MOVE '  GE' TO GODK-STATUSKODER                                      
470900     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD704 SSA1              
471000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
471100     PERFORM IMS-STATUSKONTROLL                                           
471200     .                                                                    
471300     SKIP3                                                                
471400 IMS-GU-WDD7-WDD7A-MINMAX SECTION.                                        
471500                                                                          
471600     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
471700                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
471800            DELIMITED BY SIZE INTO SSA1                                   
471900     MOVE '  GE' TO GODK-STATUSKODER                                      
472000     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-AREA-WDD7A1 SSA1              
472100     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
472200     PERFORM IMS-STATUSKONTROLL                                           
472300     .                                                                    
472400     SKIP3                                                                
472500 IMS-GN-WDD7-WDD7A-MINMAX SECTION.                                        
472600                                                                          
472700     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
472800                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
472900            DELIMITED BY SIZE INTO SSA1                                   
473000     MOVE '  GE' TO GODK-STATUSKODER                                      
473100     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-AREA-WDD7A1 SSA1              
473200     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
473300     PERFORM IMS-STATUSKONTROLL                                           
473400     .                                                                    
473500     SKIP3                                                                
473600 IMS-GN-W6D111-W6D1SEQ  SECTION.                                          
473700                                                                          
473800     STRING 'W6D111  (W6D1HSEQ =' W-W6D1HSEQ-X ')'                        
473900            DELIMITED BY SIZE INTO SSA1                                   
474000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
474100     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-AREA-W6D111 SSA1               
474200     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
474300     PERFORM IMS-STATUSKONTROLL                                           
474400     .                                                                    
474500     SKIP3                                                                
474600 IMS-GU-WDL811      SECTION.                                              
474700                                                                          
474800     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
474900          DELIMITED BY SIZE INTO SSA1                                     
475000     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
475100          DELIMITED BY SIZE INTO SSA2                                     
475200     MOVE '  GE' TO GODK-STATUSKODER                                      
475300     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-AREA-WDL811 SSA1 SSA2          
475400     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
475500     PERFORM IMS-STATUSKONTROLL                                           
475600     .                                                                    
475700     SKIP3                                                                
475800 IMS-GN-WDL811      SECTION.                                              
475900                                                                          
476000     MOVE 'WDL811  '       TO SSA1                                        
476100     MOVE '  GE' TO GODK-STATUSKODER                                      
476200     CALL CBLTDLI USING GU WDL8-PCB DLI-IO-AREA-WDL811 SSA1               
476300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
476400     PERFORM IMS-STATUSKONTROLL                                           
476500     .                                                                    
476600     SKIP3                                                                
476700 IMS-GU-WDK901      SECTION.                                              
476800                                                                          
476900     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
477000            DELIMITED BY SIZE INTO SSA1                                   
477100     MOVE '  GE' TO GODK-STATUSKODER                                      
477200     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-WDK901 SSA1               
477300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
477400     PERFORM IMS-STATUSKONTROLL                                           
477500     .                                                                    
477600     SKIP3                                                                
477700 IMS-GU-WDB601      SECTION.                                              
477800                                                                          
477900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
478000          DELIMITED BY SIZE INTO SSA1                                     
478100     MOVE '  GE' TO GODK-STATUSKODER                                      
478200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
478300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
478400     PERFORM IMS-STATUSKONTROLL                                           
478500     IF SEGMENT-SAKNAS                                                    
478600        MOVE SPACE TO DCS-KDDC                                            
478700     END-IF                                                               
478800     .                                                                    
478900     SKIP3                                                                
479000 IMS-GU-WDB616      SECTION.                                              
479100                                                                          
479200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
479300          DELIMITED BY SIZE INTO SSA1                                     
479400     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
479500          DELIMITED BY SIZE INTO SSA2                                     
479600     MOVE '  GE' TO GODK-STATUSKODER                                      
479700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
479800     MOVE WDB6-STATUS-CODE  TO STATUS-WS                                  
479900     PERFORM IMS-STATUSKONTROLL                                           
480000     .                                                                    
480100     SKIP3                                                                
471600 IMS-GU-WDA501 SECTION.                                                   
471700                                                                          
471800     STRING 'WDA501  (WDA501KY =' W-WDA501KY ')'                          
471900            DELIMITED BY SIZE INTO SSA1                                   
472000     MOVE '  GE' TO GODK-STATUSKODER                                      
472100     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-AREA-WDA501 SSA1               
472200     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
472300     PERFORM IMS-STATUSKONTROLL                                           
472400     .                                                                    
472500     SKIP3                                                                
480200 IMS-STATUSKONTROLL SECTION.                                              
480300                                                                          
480400     SET STATUS-IX TO 1                                                   
480500     SEARCH GODK-STATUS                                                   
480600       AT END                                                             
480700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
480800         DELIMITED BY SIZE INTO FELTEXT                                   
480900         CALL FELLOG                                                      
481000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
481100         CONTINUE                                                         
481200     END-SEARCH                                                           
481300     .                                                                    
481400     EJECT                                                                
481500*    -COPY WY2000P1                                                       
