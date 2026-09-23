000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5132200.                                                
000300 AUTHOR.         BOO HAMMARIN, GDC-GROUP.                                 
000400 DATE-WRITTEN.   APRIL 1997.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*    ANVÄNDA SEGMENT: WDH101                                              
001000*                     WDH111                                              
001100*                     WDK601                                              
001200*                     WDK611                                              
001300*                     WDK627                                              
001400*                     WDK701                                              
001500*                     WDK711                                              
001600*                     WDH701                                              
001700*                     WDH711                                              
001800*                     WDD311                                              
001900*                                                                         
002000*    UTDATA:     W5132D1 VECKOINFORMATION - LISTPOSTER                    
002100*                W5134N LISTPOSTER FYSISKA AVVIKELSER NDC                 
002200*                W5134P LISTPOSTER GJORDA JUSTERINGAR NDC                 
002300*                W5134R INVENTERINGSJUSTERINGAR       NDC                 
002400*                W5134S RENSNINGSPOSTER TILL WDH1                         
002500*                W51385 ERSÄTTNINGSBEVAKNING                              
002600*                W51330 LISTPOSTER FYSISKA AVVIKELSER CDC                 
002700*                W51331 LISTPOSTER GJORDA JUSTERINGAR CDC                 
002800*                W51332 LISTPOSTER FYSISKA AVVIKELSER SDC                 
002900*                W51333 LISTPOSTER GJORDA JUSTERINGAR SDC/LDC             
003000*                W51334 LISTPOSTER FYSISKA AVVIKELSER NDC PACIFIC         
003100*                W51335 LISTPOSTER GJORDA JUSTERINGAR NDC PACIFIC         
003110*                W51336 LISTPOSTER GJORDA JUSTERINGAR NDC PACIFIC         
003200*                                                                         
003300*    RETURKODER:                                                          
003400*                 999 (DUMPKOD VID FELLOG ).                              
003500     EJECT                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700 INPUT-OUTPUT SECTION.                                                    
003800 FILE-CONTROL.                                                            
003900*- - - - - - - - - - - - - - - - - - - - - - INFILER.                     
004000                                                                          
004100*- - - - - - - - - - - - - - - - - - - - - - UTFILER.                     
004200     SELECT W5132D1    ASSIGN  TO  UT-S-W51322D3.                         
004300     SELECT W5134N     ASSIGN  TO  UT-S-W51322D6.                         
004400     SELECT W5134P     ASSIGN  TO  UT-S-W51322D7.                         
004500     SELECT W5134R     ASSIGN  TO  UT-S-W51322D8.                         
004600     SELECT W5134S     ASSIGN  TO  UT-S-W51322D9.                         
004700     SELECT W51385     ASSIGN  TO  UT-S-W51322DB.                         
004800     SELECT W51330     ASSIGN  TO  UT-S-W51322DD.                         
004900     SELECT W51331     ASSIGN  TO  UT-S-W51322DE.                         
005000     SELECT W51332     ASSIGN  TO  UT-S-W51322DF.                         
005100     SELECT W51333     ASSIGN  TO  UT-S-W51322DG.                         
005200     SELECT W51334     ASSIGN  TO  UT-S-W51322DH.                         
005300     SELECT W51335     ASSIGN  TO  UT-S-W51322DI.                         
005310     SELECT W51336     ASSIGN  TO  UT-S-W51322DJ.                         
005320     SELECT W51337     ASSIGN  TO  UT-S-W51322DK.                         
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600 FILE SECTION.                                                            
005700                                                                          
005800*  LISTTRANSAKTIONER VECKA LDC/SDC/CDC                                    
005900 FD  W5132D1                                                              
006000     RECORDING V                                                          
006100     BLOCK 0 RECORDS.                                                     
006200*01  W5132D1-W5132601 -COPY W5132601 -L.                                  
006300     SKIP3                                                                
006400*01  W5132D1-W5132602 -COPY W5132602 -L.                                  
006500     EJECT                                                                
006600*  LISTINFORMATION TILL NDC                                               
006700*  ARTIKLAR MED FYSISKA AVVIKELSER                                        
006800*                                                                         
006900 FD  W5134N                                                               
007000     RECORDING F                                                          
007100     BLOCK 0 RECORDS.                                                     
007200*01  W5134N-003     -COPY W51322N  -L.                                    
007300                                                                          
007400     EJECT                                                                
007500*  LISTINFORMATION TILL NDC                                               
007600*  GJORDA JUSTERINGAR                                                     
007700*                                                                         
007800 FD  W5134P                                                               
007900     RECORDING F                                                          
008000     BLOCK 0 RECORDS.                                                     
008100*01  W5134P-009     -COPY W51322P  -L.                                    
008200     EJECT                                                                
008300                                                                          
008400*  LISTINFORMATION TILL NDC                                               
008500*  INVENTERINGSJUSTERINGAR                                                
008600*                                                                         
008700 FD  W5134R                                                               
008800     RECORDING V                                                          
008900     BLOCK 0 RECORDS.                                                     
009000*01  W5134R-01      -COPY W51322R1 -L.                                    
009100     EJECT                                                                
009200                                                                          
009300*  RENSNINGSTRANSAKTIONER TILL WDH1                                       
009400*                                                                         
009500 FD  W5134S                                                               
009600     RECORDING F                                                          
009700     BLOCK 0 RECORDS.                                                     
009800*01  W5134S-UT      -COPY W51322S  -L.                                    
009900     EJECT                                                                
010000 FD  W51385                                                               
010100     RECORDING F                                                          
010200     BLOCK 0.                                                             
010300*01  POST -COPY W51385     -PRE ERSBEV-  -L                               
010400     SKIP2                                                                
010500                                                                          
010600 FD  W51330                                                               
010700     RECORDING F                                                          
010800     BLOCK 0.                                                             
010900*01  W51330-POST -COPY W51334     -L                                      
011000                                                                          
011100 FD  W51331                                                               
011200     RECORDING F                                                          
011300     BLOCK 0.                                                             
011400*01  W51331-POST -COPY W51335     -L                                      
011500                                                                          
011600 FD  W51332                                                               
011700     RECORDING F                                                          
011800     BLOCK 0.                                                             
011900*01  W51332-POST -COPY W51334     -L                                      
012000                                                                          
012100 FD  W51333                                                               
012200     RECORDING F                                                          
012300     BLOCK 0.                                                             
012400*01  W51333-POST -COPY W51335     -L                                      
012500                                                                          
012600 FD  W51334                                                               
012700     RECORDING F                                                          
012800     BLOCK 0.                                                             
012900*01  W51334-POST -COPY W51334     -L                                      
013000                                                                          
013100 FD  W51335                                                               
013200     RECORDING F                                                          
013300     BLOCK 0.                                                             
013400*01  W51335-POST -COPY W51335     -L                                      
013410 FD  W51336                                                               
013420     RECORDING F                                                          
013430     BLOCK 0.                                                             
013440*01  W51336-POST -COPY W51335     -L                                      
013450                                                                          
013460 FD  W51337                                                               
013470     RECORDING F                                                          
013480     BLOCK 0.                                                             
013491                                                                          
013492 01  W51337-SF           PIC X(820).                                      
013600 WORKING-STORAGE SECTION.                                                 
013700                                                                          
013800 77  IDPGM                   PIC X(8)            VALUE 'W5132200'.        
013900 77  JA                      PIC X               VALUE 'J'.               
014000 77  NEJ                     PIC X               VALUE 'N'.               
014100 77  ARTSEGM-FINNS           PIC X               VALUE 'N'.               
014200 77  ARTWDK7-FINNS           PIC X               VALUE 'N'.               
014210 77  W51337-HEADER-SW        PIC X               VALUE 'N'.               
014220     88  HEADER-OK                               VALUE 'J'.               
014230                                                                          
014300                                                                          
014400*    --- FÄLT FÖR DATUMKORT                                               
014500 01  FAELT-FOR-DATUMKORT.                                                 
014600     03  FLTA                PIC X(6) VALUE 'W51322'.                     
014700     03  FLTB                PIC X(6) VALUE 'WDATUM'.                     
014800     03 -COPY WDATKORT                                                    
014900                                                                          
015000 01  FLAGGOR.                                                             
015100     03  INVJUST-FINNS       PIC X.                                       
015200                                                                          
015300 01  SPAR-AREA.                                                           
015400     03  SPAR-TISEGKEY       PIC 9(9).                                    
015500                                                                          
015510 01  SF-AREA.                                                             
015520     03  SF-TISEGKEY         PIC 9(9).                                    
015530                                                                          
015600 01  CHKP-VAR.                                                            
015700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
015800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
015900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
016000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
016100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
016200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
016300                                                                          
016400 01  DIVERSE.                                                             
016500     03  BEN-SV              PIC X(25)           VALUE  'S  '.            
016600     03  BEN-ENG             PIC X(25)           VALUE  'E  '.            
016700     03  W-KDERS-UTG         PIC S9(3) COMP-3    VALUE ZERO.              
016800     03  MAX-SORTVAERDE      PIC S9(9)V99  VALUE 999999999.99.            
016900     EJECT                                                                
017000 01  W-ADARTADR-1-SF.                                                     
017100     03  W-ADLAGOMR-1-SF     PIC 99.                                      
017200     03  W-ADGANG-1-SF       PIC 99.                                      
017300     03  W-ADPLATS-1-SF      PIC 9(5).                                    
017310 01  W-ADARTADR.                                                          
017320     03  W-ADLAGOMR          PIC 99.                                      
017330     03  W-ADGANG            PIC 99.                                      
017340     03  W-ADPLATS           PIC 9(5).                                    
017400 01  FILLER REDEFINES W-ADARTADR.                                         
017500     03  UT-ADARTADR         PIC 9(9).                                    
017501                                                                          
017700 01  FILLER                  PIC X(12)   VALUE 'DAGENS DATUM'.            
017800 01  W-DAGNR-AAMMDD          PIC 9(6).                                    
017900 01  FILLER REDEFINES W-DAGNR-AAMMDD.                                     
018000     03  W-AAR-AAMMDD        PIC 99.                                      
018100     03  W-MAAN-AAMMDD       PIC 99.                                      
018200     03  W-DAG-AAMMDD        PIC 99.                                      
018300                                                                          
018400 01  W-DAGNR-AAVVD           PIC 9(5).                                    
018500 01  FILLER REDEFINES W-DAGNR-AAVVD.                                      
018600     03  DENNA-VECKA         PIC 9(4).                                    
018700     03  FILLER REDEFINES DENNA-VECKA.                                    
018800         05  W-AAR-AAVVD         PIC 99.                                  
018900         05  W-VECKA-AAVVD       PIC 99.                                  
019000     03  W-DAG-AAVVD         PIC 9.                                       
019100                                                                          
019200 01  MINUS-VECKA             PIC 9(4).                                    
019300 01  FILLER REDEFINES MINUS-VECKA.                                        
019400     03 W-MINUS-AAR          PIC 9(2).                                    
019500     03 W-MINUS-VECKA        PIC 9(2).                                    
019600                                                                          
019700 01  W-DATUM.                                                             
019800     03  FILLER              PIC 9                   VALUE ZERO.          
019900     03  W-AAR               PIC 99.                                      
020000     03  W-MAANAD            PIC 99.                                      
020100     03  W-DAG               PIC 99.                                      
020200 01  DAGENS-DATUM REDEFINES W-DATUM PIC S9(7).                            
020300                                                                          
020400 01  W-SSAAMMDD.                                                          
020500     03  W-SS                PIC 9(2).                                    
020600     03  W-AAMMDD            PIC 9(6).                                    
020700                                                                          
020800 01  W-TID.                                                               
020900     03  DAGENS-TID          PIC 9(8)    VALUE ZERO.                      
021000                                                                          
021100 01  WS-DATUM.                                                            
021200     03  WS-DAT              PIC 9(6).                                    
021300                                                                          
021400 01  DATUM-FAELT.                                                         
021500     03  WS-JUSTDA           PIC 9(8).                                    
021600     03  FILLER  REDEFINES  WS-JUSTDA.                                    
021700      05 WS-JUSTSEKEL        PIC 9(2).                                    
021800      05 WS-JUSTDAT          PIC 9(6).                                    
021900      05 FILLER  REDEFINES  WS-JUSTDAT.                                   
022000       07 WS-JUSTAAR         PIC 9(2).                                    
022100       07 FILLER             PIC 9(4).                                    
022200                                                                          
022300 01  NOLLOR.                                                              
022400     03  NOLL-ADRESS         PIC S9(5)   COMP-3 OCCURS 5.                 
022500                                                                          
022600     EJECT                                                                
022700                                                                          
022800 01  FILLER PIC X(16) VALUE '*****NYCKLAR****'.                           
022900                                                                          
023000 01  ARTNR.                                                               
023100     03  ARTNR-SEARCH            PIC S9(9)   COMP-3.                      
023200     SKIP2                                                                
023300 01  W-IDARTNR-X.                                                         
023400     03  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
023500     SKIP2                                                                
023600 01  W-TISEGKEY-X.                                                        
023700     03  W-TISEGKEY          PIC S9(9)  VALUE +999999999 COMP-3.          
023800     SKIP2                                                                
023900 01  W-KDERS-X.                                                           
024000     03  FILLER                  PIC S9(3)   COMP-3  VALUE +0.            
024100     SKIP2                                                                
024200 01  KEYWDH1-MIN.                                                         
024300     03  IDDC-MIN.                                                        
024400         05  IDDC-SEARCH-MIN PIC X(2)   VALUE '00'.                       
024500     03  INVKAT-SEARCH-MIN   PIC S9(3)  VALUE ZERO COMP-3.                
024600     03  TISEGKEY-SEARCH-MIN PIC S9(9)  VALUE ZERO COMP-3.                
024700     03  DAREGDAT-SORT-MIN PIC 9(8)     VALUE ZERO.                       
024800                                                                          
024900 01  KEYWDH1-MAX.                                                         
025000     03  IDDC-MAX.                                                        
025100         05  IDDC-SEARCH-MAX PIC X(2)   VALUE '99'.                       
025200     03  INVKAT-SEARCH-MAX   PIC S9(3)  VALUE +999       COMP-3.          
025300     03  TISEGKEY-SEARCH-MAX PIC S9(9)  VALUE +999999999 COMP-3.          
025400     03  DAREGDAT-SORT-MAX PIC 9(8)     VALUE 99999999.                   
025500                                                                          
025600 01  W-INVNYCK-X.                                                         
025700     03  W-IDHTYP                PIC X(4)      VALUE '5111'.              
025800     03  W-IDDC-3                PIC X(2)      VALUE SPACE.               
025900     03  FILLER                  PIC X(24)     VALUE LOW-VALUE.           
026000                                                                          
026100 01  W-IDSKYLT-X.                                                         
026200     03  W-IDSKYLT               PIC X(3)      VALUE 'GB '.               
026300                                                                          
026400 01  W-IDDC-X.                                                            
026500     03  W-IDDC                  PIC X(2)      VALUE SPACE.               
026600                                                                          
026700 01  W-IDDC-B6-X.                                                         
026800     03 W-IDDC-B6                PIC X(2).                                
026900                                                                          
027000     EJECT                                                                
027010 01  RUBRIK1.                                                             
027080     03  FILLER         PIC X(11) VALUE 'PART_NUMBER'.                    
027090     03  FILLER         PIC X      VALUE ';'.                             
027091     03  FILLER         PIC X(16) VALUE 'PART_DESCRIPTION'.               
027092     03  FILLER         PIC X      VALUE ';'.                             
027093     03  FILLER         PIC X(19) VALUE 'DISTRUBUTION_CENTER'.            
027094     03  FILLER         PIC X      VALUE ';'.                             
027095     03  FILLER         PIC X(20) VALUE 'STOCKTAKING_CATEGORY'.           
027096     03  FILLER         PIC X      VALUE ';'.                             
027097     03  FILLER         PIC X(18) VALUE 'VOLUME_VALUE_CLASS'.             
027098     03  FILLER         PIC X      VALUE ';'.                             
027099     03  FILLER         PIC X(17) VALUE 'ADJUSTED_QUANTITY'.              
027100     03  FILLER         PIC X      VALUE ';'.                             
027103     03  FILLER         PIC X(15) VALUE 'INVENTORY_NOTES'.                
027104     03  FILLER         PIC X      VALUE ';'.                             
027105     03  FILLER         PIC X(22) VALUE 'INVENTORY_REQUEST_DATE'.         
027106     03  FILLER         PIC X      VALUE ';'.                             
027107     03  FILLER         PIC X(20) VALUE 'AUTOMATIC_ADJUSTMENT'.           
027108     03  FILLER         PIC X      VALUE ';'.                             
027109     03  FILLER         PIC X(15) VALUE 'ADJUSTMENT_TYPE'.                
027110     03  FILLER         PIC X      VALUE ';'.                             
027111     03  FILLER         PIC X(13) VALUE 'PRODUCT_GROUP'.                  
027112     03  FILLER         PIC X      VALUE ';'.                             
027113     03  FILLER         PIC X(09) VALUE 'SORT_CODE'.                      
027114     03  FILLER         PIC X      VALUE ';'.                             
027115     03  FILLER         PIC X(18) VALUE 'PROCUREMENT_NUMBER'.             
027116     03  FILLER         PIC X      VALUE ';'.                             
027117     03  FILLER         PIC X(20) VALUE 'STOCK_ACCOUNT_NUMBER'.           
027118     03  FILLER         PIC X      VALUE ';'.                             
027119     03  FILLER         PIC X(17) VALUE 'SUPERSESSION_CODE'.              
027120     03  FILLER         PIC X      VALUE ';'.                             
027121     03  FILLER         PIC X(18) VALUE 'ADVICED_NOT_BINNED'.             
027122     03  FILLER         PIC X(13) VALUE '_QUANTITY_CDC'.                  
027123     03  FILLER         PIC X      VALUE ';'.                             
027124     03  FILLER         PIC X(18) VALUE 'ADVICED_NOT_BINNED'.             
027125     03  FILLER         PIC X(18) VALUE '_QUANTITY_TERMINAL'.             
027126     03  FILLER         PIC X      VALUE ';'.                             
027127     03  FILLER         PIC X(14) VALUE 'STANDARD_PRICE'.                 
027128     03  FILLER         PIC X      VALUE ';'.                             
027130     03  FILLER         PIC X(18) VALUE 'AVERAGE_COST_PRICE'.             
027131     03  FILLER         PIC X      VALUE ';'.                             
027132     03  FILLER         PIC X(18) VALUE 'ADVICED_NOT_BINNED'.             
027133     03  FILLER         PIC X(13) VALUE '_QUANTITY_SDC'.                  
027134     03  FILLER         PIC X      VALUE ';'.                             
027135     03  FILLER         PIC X(17) VALUE 'WAREHOUSE_ADDRESS'.              
027136     03  FILLER         PIC X      VALUE ';'.                             
027137     03  FILLER         PIC X(04) VALUE 'AREA'.                           
027138     03  FILLER         PIC X      VALUE ';'.                             
027139     03  FILLER         PIC X(16) VALUE 'GOODS_IN_TRANSIT'.               
027140     03  FILLER         PIC X      VALUE ';'.                             
027141     03  FILLER         PIC X(21) VALUE 'NOT_INVOICED_QUANTITY'.          
027142     03  FILLER         PIC X      VALUE ';'.                             
027143     03  FILLER         PIC X(14) VALUE 'STOCK_QUANTITY'.                 
027144     03  FILLER         PIC X      VALUE ';'.                             
027145     03  FILLER         PIC X(17) VALUE 'RESERVED_QUANTITY'.              
027146     03  FILLER         PIC X      VALUE ';'.                             
027147     03  FILLER         PIC X(15) VALUE 'ADJUSTMENT_DATE'.                
027148     03  FILLER         PIC X      VALUE ';'.                             
027149     03  FILLER         PIC X(08) VALUE 'SUPPLIER'.                       
027150     03  FILLER         PIC X      VALUE ';'.                             
027160     03  FILLER         PIC X(21) VALUE 'STOCKTAKING_TREATMENT'.          
027170     03  FILLER         PIC X(05) VALUE '_FLAG'.                          
027180     03  FILLER         PIC X      VALUE ';'.                             
027181     03  FILLER         PIC X(19) VALUE 'STOCKTAKING_WRITTEN'.            
027182     03  FILLER         PIC X(05) VALUE '_FLAG'.                          
027183     03  FILLER         PIC X      VALUE ';'.                             
027184     03  FILLER         PIC X(19) VALUE 'PHYSICAL_DEVIATIONS'.            
027185     03  FILLER         PIC X(13) VALUE '_CDC_SDC_FLAG'.                  
027186     03  FILLER         PIC X      VALUE ';'.                             
027187     03  FILLER         PIC X(19) VALUE 'ADJUSTMENTS_CDC_SDC'.            
027188     03  FILLER         PIC X(05) VALUE '_FLAG'.                          
027189     03  FILLER         PIC X      VALUE ';'.                             
027190     03  FILLER         PIC X(21) VALUE 'WEEKLY_REPORT_CDC_SDC'.          
027191     03  FILLER         PIC X(18) VALUE '_TRANSACTIONS_FLAG'.             
027192     03  FILLER         PIC X      VALUE ';'.                             
027193     03  FILLER         PIC X(17) VALUE 'NUMBER_PER_PERIOD'.              
027194     03  FILLER         PIC X(05) VALUE '_FLAG'.                          
027195     03  FILLER         PIC X      VALUE ';'.                             
027196     03  FILLER         PIC X(19) VALUE 'PHYSICAL_DEVIATIONS'.            
027197     03  FILLER         PIC X(09) VALUE '_NDC_FLAG'.                      
027198     03  FILLER         PIC X      VALUE ';'.                             
027199     03  FILLER         PIC X(15) VALUE 'ADJUSTMENTS_NDC'.                
027200     03  FILLER         PIC X(05) VALUE '_FLAG'.                          
027201     03  FILLER         PIC X      VALUE ';'.                             
027202     03  FILLER         PIC X(18) VALUE 'WEEKLY_REPORT_NDC_'.             
027203     03  FILLER         PIC X(17) VALUE 'TRANSACTIONS_FLAG'.              
027204     03  FILLER         PIC X      VALUE ';'.                             
027205     03  FILLER         PIC X(17) VALUE 'SUBSTITUTE_STOCK_'.              
027206     03  FILLER         PIC X(11) VALUE 'AMOUNT_FLAG'.                    
027207     03  FILLER         PIC X      VALUE ';'.                             
027208     03  FILLER         PIC X(11) VALUE 'RECORD_DATE'.                    
027209 01  GENERELLA-SUBPROGRAM.                                                
027210     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
027300     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
027400     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM '.          
027500     03  DATKORT                 PIC X(8)      VALUE 'DATKORT '.          
027600     03  ABEND                   PIC X(8)      VALUE 'ABEND   '.          
027700                                                                          
027800*---- PARAMETRAR TILL ABEND                                               
027900 01  RETURKODER.                                                          
028000     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
028100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
028200     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
028300                                                                          
028400     EJECT                                                                
028500 01  FILLER                      PIC X(16) VALUE '***W5132D1****'.        
028600 01  WORKAREA-W5132D1.                                                    
028700*    03  -COPY W5132601 -PRE W26-1-.                                      
028800     EJECT                                                                
028900*    03  -COPY W5132602 -PRE W26-2-.                                      
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16) VALUE 'AREA FYS AVVIK'.        
029200 01  WORKAREA-W51334.                                                     
029300*    03  AREA -COPY W51334   -PRE FYS-                                    
029400     EJECT                                                                
029500 01  FILLER                      PIC X(16) VALUE 'JUST-AREA'.             
029600 01  WORKAREA-W51335.                                                     
029700*    03  AREA -COPY W51335   -PRE JUST-.                                  
029800     EJECT                                                                
029900 01  FILLER                      PIC X(16) VALUE '*****W5134N'.           
030000*01  -COPY W51322N  -PRE L3-NDC-.                                         
030100     EJECT                                                                
030200 01  FILLER                      PIC X(16) VALUE '*****W5134P'.           
030300*01  -COPY W51322P  -PRE L9-NDC-.                                         
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16) VALUE '*****W5134R'.           
030600*01  -COPY W51322R1 -PRE W42R1-.                                          
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16) VALUE '*****W5134S'.           
030900*01  -COPY W51322S  -PRE W42S-.                                           
031000     EJECT                                                                
031010 01  WORKAREA-W51337.                                                     
031026     03  W-IDARTNR-SF        PIC 9(10).                                   
031027     03  FILLER              PIC X    VALUE ';'.                          
031028     03  W-BEART-SF          PIC X(25).                                   
031029     03  FILLER              PIC X    VALUE ';'.                          
031030     03  W-IDDC-SF           PIC X(2).                                    
031031     03  FILLER              PIC X    VALUE ';'.                          
031032     03  W-KDINVKAT-SF       PIC 9(2).                                    
031033     03  FILLER              PIC X    VALUE ';'.                          
031034     03  W-KDVVKL-SF         PIC 9(2).                                    
031035     03  FILLER              PIC X    VALUE ';'.                          
031036     03  W-KVJUSTKV-SF       PIC -(7)9.                                   
031037     03  FILLER              PIC X    VALUE ';'.                          
031040     03  W-TEINVANM-SF       PIC X(25).                                   
031041     03  FILLER              PIC X    VALUE ';'.                          
031042     03  W-TIM-INV-SF        PIC 9(6).                                    
031043     03  FILLER              PIC X    VALUE ';'.                          
031044     03  W-FLAUTLSJ-SF       PIC X.                                       
031045     03  FILLER              PIC X    VALUE ';'.                          
031046     03  W-KDJUSTYP-SF       PIC 9(2).                                    
031047     03  FILLER              PIC X    VALUE ';'.                          
031048     03  W-KDPRODSL-SF       PIC 9(2).                                    
031049     03  FILLER              PIC X    VALUE ';'.                          
031050     03  W-KDSORT-SF         PIC X(2).                                    
031051     03  FILLER              PIC X    VALUE ';'.                          
031052     03  W-IDANSK-SF         PIC 9(3).                                    
031053     03  FILLER              PIC X    VALUE ';'.                          
031054     03  W-IDLKTO-SF         PIC X(10).                                   
031055     03  FILLER              PIC X    VALUE ';'.                          
031056     03  W-KDERS-SF          PIC 9(2).                                    
031057     03  FILLER              PIC X    VALUE ';'.                          
031058     03  W-KVAKS-CDC-SF      PIC -(7)9.                                   
031059     03  FILLER              PIC X    VALUE ';'.                          
031060     03  W-KVAKS-T-SF        PIC -(7)9.                                   
031061     03  FILLER              PIC X    VALUE ';'.                          
031062     03  W-PRARTSTD-SF       PIC -(7)9.9(2).                              
031063     03  FILLER              PIC X    VALUE ';'.                          
031064     03  W-PRAVCOST-SF       PIC -(7)9.9(2).                              
031065     03  FILLER              PIC X    VALUE ';'.                          
031066     03  W-KVAKS-SDC-SF      PIC -(7)9.                                   
031067     03  FILLER              PIC X    VALUE ';'.                          
031068     03  W-ADARTADR-SF       PIC 9(9).                                    
031069     03  FILLER              PIC X    VALUE ';'.                          
031070     03  W-ADLAGOMR-SF       PIC 9(2).                                    
031071     03  FILLER              PIC X    VALUE ';'.                          
031072     03  W-KVAKS-PAV-SF      PIC -(7)9.                                   
031073     03  FILLER              PIC X    VALUE ';'.                          
031074     03  W-KVEFRS-SF         PIC -(6)9.                                   
031075     03  FILLER              PIC X    VALUE ';'.                          
031076     03  W-KVLS-SF           PIC -(6)9.                                   
031077     03  FILLER              PIC X    VALUE ';'.                          
031078     03  W-KVUTRS-SF         PIC -(6)9.                                   
031079     03  FILLER              PIC X    VALUE ';'.                          
031080     03  W-TIJUSTDA-SF       PIC 9(6).                                    
031081     03  FILLER              PIC X    VALUE ';'.                          
031082     03  W-IDLEVNR-SF        PIC X(6).                                    
031103     03  FILLER              PIC X    VALUE ';'.                          
031104     03  W-FLINVBEH-SF       PIC X.                                       
031105     03  FILLER              PIC X    VALUE ';'.                          
031106     03  W-FLINVSKR-SF       PIC X.                                       
031107     03  FILLER              PIC X    VALUE ';'.                          
031108     03  W-FLINV2B-SF        PIC X.                                       
031109     03  FILLER              PIC X    VALUE ';'.                          
031110     03  W-FLINV2C-SF        PIC X.                                       
031111     03  FILLER              PIC X    VALUE ';'.                          
031112     03  W-FLINV2D-SF        PIC X.                                       
031113     03  FILLER              PIC X    VALUE ';'.                          
031114     03  W-FLINV3E-SF        PIC X.                                       
031115     03  FILLER              PIC X    VALUE ';'.                          
031116     03  W-FLINV4N-SF        PIC X.                                       
031117     03  FILLER              PIC X    VALUE ';'.                          
031118     03  W-FLINV4P-SF        PIC X.                                       
031119     03  FILLER              PIC X    VALUE ';'.                          
031120     03  W-FLINV4R-SF        PIC X.                                       
031121     03  FILLER              PIC X    VALUE ';'.                          
031122     03  W-FLINV85-SF        PIC X.                                       
031123     03  FILLER              PIC X    VALUE ';'.                          
031124     03  W-DATUM-SF          PIC X(6).                                    
031125     EJECT                                                                
031130*--------------------------------------- AREA FÖR W51385-POST             
031200                                                                          
031300*01  AREA -COPY W51385     -PRE ERSBEV-                                   
031400     EJECT                                                                
031500*-----------------------------------------VALID IDDC CODES                
031600*01  -COPY WWDC99                                                         
031700                                                                          
031800*-----------------------------------------PARAMETRAR TILL                 
031900*                                         SUBPROGRAM POSTSUM              
032000 01  FILLER             PIC X(7)   VALUE 'POSTSUM'.                       
032100*01  -COPY W0005 -PRE POSTSUM-.                                           
032200     EJECT                                                                
032300*    SPARAREOR FÖR DATABASSEGMENT                                         
032400*                                                                         
032500 01  FILLER             PIC X(16) VALUE '*****SB02-WDH111'.               
032600*01  WDH111    -COPY WDH111  -PRE SB02-.                                  
032700     EJECT                                                                
032800 01  FILLER             PIC X(16) VALUE '*****SB02-WDH121'.               
032900*01  WDH121    -COPY WDH121  -PRE SB02-.                                  
033000     EJECT                                                                
033100 01  FILLER                  PIC X(16)   VALUE '   WLARTC01'.             
033200*01  WLARTC01  -COPY WDK601 -PRE BE01-.                                   
033300     EJECT                                                                
033400 01  FILLER                  PIC X(16)   VALUE '   WLARTC11'.             
033500*01  WLARTC11  -COPY WDK611 -PRE BE11-.                                   
033600     EJECT                                                                
033700 01  FILLER                  PIC X(16)   VALUE '   WLARTC27'.             
033800*01  WLARTC27  -COPY WDK627 -PRE BE27-.                                   
033900     EJECT                                                                
034000 01  FILLER                  PIC X(16)   VALUE '   WLARTS01'.             
034100*01  WLARTS01  -COPY WDK701 -PRE SE01-.                                   
034200     EJECT                                                                
034300 01  FILLER                  PIC X(16)   VALUE '   WLARTS11'.             
034400*01  WLARTS11  -COPY WDK711 -PRE SE11-.                                   
034500     EJECT                                                                
034600*****                                                                     
034700*****    IN-AREA TILL IMS-SEKTIONERNA                                     
034800*****                                                                     
034900 01  IMS-WORKAREOR.                                                       
035000     03  FILLER          PIC X(16)   VALUE '*-*-*IMS-WS*-*-*'.            
035100     03  STATUS-WS       PIC XX.                                          
035200         88  SEGMENT-FINNS       VALUE '  '.                              
035300         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
035400         88  BASEN-SLUT          VALUE 'GB'.                              
035500         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
035600     SKIP3                                                                
035700     03  GODK-STATUSKODER.                                                
035800         05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.            
035900     SKIP3                                                                
036000 01      SSA1            PIC X(128).                                      
036100 01      SSA2            PIC X(64).                                       
036200     EJECT                                                                
036300*                                                                         
036400*        IMS FUNKTIONSKODER                                               
036500*                                                                         
036600*01      -COPY W0003                                                      
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER              PIC X(16)      VALUE  'WDH101'.                  
037000*                                                                         
037100*01  WDH101      -COPY WDH101                                             
037200     EJECT                                                                
037300****************************************                                  
037400                                                                          
037500 01  FILLER              PIC X(16)      VALUE  'WLBENA11'.                
037600*                                                                         
037700*01  WLBENA11 -COPY WDD311 -PRE BEN-                                      
037800     EJECT                                                                
037900                                                                          
038000 01  FILLER              PIC X(16)      VALUE  'WLINVC01'.                
038100*                                                                         
038200*01  WLINVC01 -COPY WDH701                                                
038300     EJECT                                                                
038400 01  FILLER              PIC X(16)      VALUE  'WLINVC11'.                
038500*                                                                         
038600*01  WLINVC11 -COPY WDH711                                                
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
039000 01   DLI-IO-AREA-B601.                                                   
039100*     03  -COPY WDB601                                                    
039200                                                                          
039300     EJECT                                                                
039400 LINKAGE SECTION.                                                         
039500*  WDH1                                                                   
039600*01  -COPY W0008  -PRE WDH1-                                              
039700     05  WDH1-KONKAT-KEY PIC X(10).                                       
039800                                                                          
039900*  WDK6                                                                   
040000*01  -COPY W0008  -PRE WLARTC-.                                           
040100     05  WLARTC-KONKAT-KEY PIC X(10).                                     
040200     EJECT                                                                
040300*  WDD3                                                                   
040400*01  -COPY W0008  -PRE BEN-.                                              
040500     05  BEN-KONKAT-KEY PIC X(30).                                        
040600                                                                          
040700*  WDK7                                                                   
040800*01  -COPY W0008  -PRE WLARTS-.                                           
040900     05  WLARTS-KONKAT-KEY PIC X(30).                                     
041000                                                                          
041100     EJECT                                                                
041200*  WDH7                                                                   
041300*01  -COPY W0008  -PRE WLINVC-.                                           
041400     05  WLINVC-KONKAT-KEY PIC X(30).                                     
041500                                                                          
041600*01  -COPY W0008  -PRE WDB6-                                              
041700     05  FILLER            PIC X.                                         
041800                                                                          
041900                                                                          
042000     EJECT                                                                
042100 PROCEDURE DIVISION USING WDH1-PCB                                        
042200                          WLARTC-PCB                                      
042300                          BEN-PCB                                         
042400                          WLARTS-PCB                                      
042500                          WLINVC-PCB                                      
042600                          WDB6-PCB.                                       
042700 MAIN SECTION.                                                            
042800     ENTRY 'DLITCBL' USING WDH1-PCB                                       
042900                           WLARTC-PCB                                     
043000                           BEN-PCB                                        
043100                           WLARTS-PCB                                     
043200                           WLINVC-PCB                                     
043300                           WDB6-PCB.                                      
043400                                                                          
043500     PERFORM A-INITIERING                                                 
043600                                                                          
043700     PERFORM B-BEARBETNING                                                
043800     PERFORM C-AVSLUTNING                                                 
043900                                                                          
044000     MOVE +0 TO RETURN-CODE                                               
044100     GOBACK                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 A-INITIERING SECTION.                                                    
044500                                                                          
044600     CALL DATKORT USING FLTA  FLTB  DATUMKORT                             
044700     MOVE D-VECKA        TO W-VECKA-AAVVD                                 
044800     MOVE D-DAGNR        TO W-DAG-AAVVD                                   
044900     MOVE D-AAR          TO W-AAR-AAVVD                                   
045000                                                                          
045100     ACCEPT DAGENS-TID   FROM TIME                                        
045200     ACCEPT WS-DAT       FROM DATE                                        
045300                                                                          
045400     MOVE WS-DAT         TO W-DAGNR-AAMMDD                                
045500                                                                          
045600     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
045700                                                                          
045800     OPEN OUTPUT W5132D1                                                  
045900                 W51330                                                   
046000                 W51331                                                   
046100                 W51332                                                   
046200                 W51333                                                   
046300                 W51334                                                   
046400                 W51335                                                   
046410                 W51336                                                   
046500                 W5134N                                                   
046600                 W5134P                                                   
046700                 W5134R                                                   
046800                 W5134S                                                   
046900                 W51385                                                   
046910                 W51337                                                   
047000     .                                                                    
047100     EJECT                                                                
047200 B-BEARBETNING SECTION.                                                   
047300                                                                          
047400     PERFORM IMS-GET-INVENTERINGSROT                                      
047500                                                                          
047600     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
047700                                                                          
047710       INITIALIZE WORKAREA-W51337                                         
047720                                                                          
047800       MOVE ART-IDARTNR IN WDH101 TO ARTNR-SEARCH                         
047900                                       W-IDARTNR                          
047910                                       W-IDARTNR-SF                       
048000                                                                          
048100       PERFORM IMS-GET-INVENTERINGSINF                                    
048200       IF SEGMENT-FINNS                                                   
048300         PERFORM UNTIL SEGMENT-SAKNAS                                     
048400           MOVE SB02-INV-IDDC  TO W-IDDC-3                                
048500                                  WS-IDDC                                 
048600                                  W-IDDC                                  
048700                                  W-IDDC-B6                               
048710                                  W-IDDC-SF                               
048800           PERFORM IMS-GU-WDB601                                          
048900                                                                          
049000           PERFORM BB-PUNKT-1-6                                           
049100           PERFORM IMS-GET-INVENTERINGSINF                                
049200         END-PERFORM                                                      
049250       CALL POSTSUM USING POSTSUM-PARM                                    
049300       END-IF                                                             
049400       PERFORM IMS-GET-INVENTERINGSROT                                    
049500                                                                          
049600     END-PERFORM                                                          
049700     .                                                                    
049800     EJECT                                                                
049900 BB-PUNKT-1-6 SECTION.                                                    
050000*    SKAPAR INVENTERINGAR FRÅN INVENTERINGSBASEN.                         
050100                                                                          
050110     MOVE SB02-INV-KDINVKAT TO W-KDINVKAT-SF                              
050120     MOVE SB02-INV-KDVVKL   TO W-KDVVKL-SF                                
050130     MOVE SB02-INV-KVJUSTKV TO W-KVJUSTKV-SF                              
050140     MOVE SB02-INV-TEINVANM TO W-TEINVANM-SF                              
050150     MOVE SB02-INV-TISEGKEY TO SF-TISEGKEY                                
050152     MOVE SF-TISEGKEY(3:6)  TO W-TIM-INV-SF                               
050153                               W-TIJUSTDA-SF                              
050154     MOVE SB02-INV-FLINVBEH TO W-FLINVBEH-SF                              
050155     MOVE SB02-INV-FLINVSKR TO W-FLINVSKR-SF                              
050156     MOVE SB02-INV-FLINV2B  TO W-FLINV2B-SF                               
050157     MOVE SB02-INV-FLINV2C  TO W-FLINV2C-SF                               
050158     MOVE SB02-INV-FLINV2D  TO W-FLINV2D-SF                               
050159     MOVE SB02-INV-FLINV3E  TO W-FLINV3E-SF                               
050160     MOVE SB02-INV-FLINV4N  TO W-FLINV4N-SF                               
050161     MOVE SB02-INV-FLINV4P  TO W-FLINV4P-SF                               
050162     MOVE SB02-INV-FLINV4R  TO W-FLINV4R-SF                               
050163     MOVE SB02-INV-FLINV85  TO W-FLINV4R-SF                               
050170                                                                          
050200     MOVE NEJ           TO ARTSEGM-FINNS                                  
050300                           INVJUST-FINNS                                  
050400     IF NDC-NA AND DCS-FLWEBDC = NEJ                                      
050500       PERFORM S02-LAES-ARTREG-WDK7-NDC                                   
050600     ELSE                                                                 
050700       IF CDC-SE                                                          
050800         PERFORM S01-LAES-ARTREG-WDK6-CDC                                 
050900       ELSE                                                               
051000         PERFORM S03-LAES-ARTREG-WDK7-SDC                                 
051100       END-IF                                                             
051200     END-IF                                                               
051300                                                                          
051400     IF ARTSEGM-FINNS = JA                                                
051500       PERFORM S05-LAES-BENAEMN                                           
051600                                                                          
051700       IF SB02-INV-FLINV85 = JA                                           
051800          PERFORM S85-SKRIV-ERS-BEVAKNING                                 
051900       END-IF                                                             
052000       IF CDC-SE                                                          
052100         PERFORM BBA-CDC-SCHEDULE                                         
052200       ELSE                                                               
052300         IF DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN                           
052400         OR DCS-NDC-NA OR DCS-NDC-OTHERS OR DCS-NDC-SA                    
052500           PERFORM BBC-SDC-SCHEDULE                                       
052600           IF NDC-NA AND DCS-FLWEBDC = NEJ                                
052700             PERFORM BBB-NDC-SCHEDULE                                     
052800           END-IF                                                         
052900         END-IF                                                           
053000       END-IF                                                             
053100     ELSE                                                                 
053200       PERFORM S07-SKRIV-RENSFIL                                          
053300     END-IF                                                               
053310     IF W51337-HEADER-SW = 'N'                                            
053320       WRITE W51337-SF  FROM RUBRIK1                                      
053330       SET HEADER-OK    TO TRUE                                           
053340     END-IF                                                               
053341     MOVE WS-DAT        TO W-DATUM-SF                                     
053350     WRITE W51337-SF    FROM WORKAREA-W51337                              
053360     MOVE 'W51337'      TO POSTSUM-FDNAMN                                 
053370     MOVE 'W51322DK'    TO POSTSUM-DDNAMN2                                
053380     MOVE 'SF '         TO POSTSUM-TRANSTYP                               
053400     .                                                                    
053500     EJECT                                                                
053600 BBA-CDC-SCHEDULE SECTION.                                                
053700                                                                          
053800     EVALUATE TRUE                                                        
053900                                                                          
054000     WHEN SB02-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                  
054100       IF SB02-INV-FLINVBEH = JA                                          
054200         PERFORM S07-SKRIV-RENSFIL                                        
054300       END-IF                                                             
054400                                                                          
054500     WHEN SB02-INV-KDINVKAT = +2                                          
054600       IF SB02-INV-FLINV3E  = JA AND                                      
054700          SB02-INV-FLINVBEH = JA                                          
054800         PERFORM BBA1-TRANS-UTREDN-SALDO-CDC                              
054900         PERFORM S07-SKRIV-RENSFIL                                        
055000       END-IF                                                             
055100                                                                          
055200     WHEN SB02-INV-KDINVKAT = +6                                          
055300       IF SB02-INV-FLINV2C = JA                                           
055400         PERFORM BBA4-LISTTRANS-JUSTINFO-CDC                              
055500       END-IF                                                             
055600       IF SB02-INV-FLINV2D = JA                                           
055700         PERFORM BBA3-KVANTJUST-TRANS                                     
055800       END-IF                                                             
055900       IF SB02-INV-FLINV2C = JA AND                                       
056000          SB02-INV-FLINV2D = JA                                           
056100         PERFORM S07-SKRIV-RENSFIL                                        
056200       END-IF                                                             
056300                                                                          
056400     WHEN SB02-INV-KDINVKAT = +8                                          
056500       IF SB02-INV-FLINV2D = JA                                           
056600         PERFORM BBA2-VARDEJUST-TRANS                                     
056700       END-IF                                                             
056800       IF SB02-INV-FLINV3E = JA                                           
056900         PERFORM BBA1-TRANS-UTREDN-SALDO-CDC                              
057000       END-IF                                                             
057100       IF SB02-INV-FLINV2D = JA AND                                       
057200          SB02-INV-FLINV3E = JA                                           
057300         PERFORM S07-SKRIV-RENSFIL                                        
057400       END-IF                                                             
057500                                                                          
057600     WHEN SB02-INV-KDINVKAT = +11                                         
057700       IF SB02-INV-FLINV3E = JA                                           
057800         PERFORM BBA1-TRANS-UTREDN-SALDO-CDC                              
057900         PERFORM S07-SKRIV-RENSFIL                                        
058000       END-IF                                                             
058100                                                                          
058200     WHEN SB02-INV-KDINVKAT = +12                                         
058300       IF SB02-INV-FLINV2C = JA                                           
058400         PERFORM BBA4-LISTTRANS-JUSTINFO-CDC                              
058500       END-IF                                                             
058600       IF SB02-INV-FLINV2D = JA                                           
058700         PERFORM BBA2-VARDEJUST-TRANS                                     
058800       END-IF                                                             
058900       IF SB02-INV-FLINV2C = JA AND                                       
059000          SB02-INV-FLINV2D = JA                                           
059100         PERFORM S07-SKRIV-RENSFIL                                        
059200       END-IF                                                             
059300     END-EVALUATE                                                         
059400     .                                                                    
059500     EJECT                                                                
059600 BBA1-TRANS-UTREDN-SALDO-CDC SECTION.                                     
059700                                                                          
059800*  SKAPAR LISTPOSTER TILL CDC, UPPD. UTREDN.SALDO                         
059900                                                                          
060000     MOVE WS-IDDC              TO FYS-IDDC                                
060100     MOVE BE01-ART-IDARTNR     TO FYS-IDARTNR                             
060200     MOVE 0                    TO FYS-KDSORT1                             
060300     MOVE SB02-INV-KDINVKAT    TO FYS-KDINVKAT                            
060400     MOVE UT-ADARTADR          TO FYS-ADARTADR                            
060500     MOVE BE01-ART-KDPRODSL    TO FYS-KDPRODSL                            
060600     MOVE ZERO                 TO FYS-TIAVIDAT                            
060700                                  FYS-KVAVIS                              
060800     MOVE BE11-CLAG-KVAKS-CDC  TO FYS-KVAKS                               
060900     MOVE BE11-CLAG-KVLS       TO FYS-KVLS                                
061000     MOVE BE11-CLAG-KVEFRS     TO FYS-KVEFRS                              
061100     MOVE BE11-CLAG-KVUTRS     TO FYS-KVUTRS                              
061200     MOVE BEN-SV               TO FYS-BEART                               
061300                                                                          
061400     IF INVJUST-FINNS = JA                                                
061500       MOVE INVH-KVJUSTKV      TO FYS-KVJUSTKV                            
061600       MOVE INVH-DAREGDAT-CLO  TO W-SSAAMMDD                              
061700       MOVE W-AAMMDD           TO FYS-TIJUSTDA                            
061800     ELSE                                                                 
061900       MOVE +0                 TO FYS-KVJUSTKV                            
062000                                  FYS-TIJUSTDA                            
062100     END-IF                                                               
062200                                                                          
062300     PERFORM S30-LISTA-UPD-UTRSALDO-CDC                                   
062400     .                                                                    
062500     EJECT                                                                
062600 BBA2-VARDEJUST-TRANS SECTION.                                            
062700                                                                          
062800     MOVE +2                   TO W26-1-IDLISTA                           
062900     MOVE WS-IDDC              TO W26-1-IDDC                              
063000     MOVE BE01-ART-IDARTNR     TO W26-1-IDARTNR                           
063100     MOVE BE11-CLAG-ADLAGOMR   TO W26-1-ADLAGOMR                          
063200     MOVE BEN-ENG              TO W26-1-BEART-ENG                         
063300     MOVE BE11-CLAG-PRARTSTD   TO W26-1-PRARTSTD                          
063400     MOVE BE11-CLAG-KDERS      TO W26-1-KDERS                             
063500     MOVE SB02-INV-KVJUSTKV    TO W26-1-KVJUSTKV                          
063600     MOVE SB02-INV-TISEGKEY    TO SPAR-TISEGKEY                           
063700     MOVE SPAR-TISEGKEY(3:6)   TO W26-1-TIM-INV                           
063800     MOVE BE11-CLAG-IDANSK     TO W26-1-IDANSK                            
063900     MOVE BE11-CLAG-KVLS       TO W26-1-KVLS                              
064000                                                                          
064100     MOVE BE01-ART-KDPRODSL    TO W26-1-KDPRODSL                          
064200     COMPUTE W26-1-JUST-VAERDE =                                          
064300               BE11-CLAG-PRARTSTD * SB02-INV-KVJUSTKV                     
064400                                                                          
064500     WRITE W5132D1-W5132601 FROM W26-1-W5132601                           
064600     MOVE 'W5132D1'            TO POSTSUM-FDNAMN                          
064700     MOVE 'W51322D3'           TO POSTSUM-DDNAMN2                         
064800     MOVE ' 2 '                TO POSTSUM-TRANSTYP                        
064900     CALL POSTSUM USING POSTSUM-PARM                                      
065000     .                                                                    
065100     EJECT                                                                
065200 BBA3-KVANTJUST-TRANS SECTION.                                            
065300                                                                          
065400     MOVE +1                   TO W26-2-IDLISTA                           
065500     MOVE WS-IDDC              TO W26-2-IDDC                              
065600     MOVE BE01-ART-IDARTNR     TO W26-2-IDARTNR                           
065700     MOVE SPACE                TO W26-2-SORT-BLANK                        
065800     MOVE SB02-INV-KVJUSTKV    TO W26-2-KVJUSTKV                          
065900     MOVE SB02-INV-TISEGKEY    TO SPAR-TISEGKEY                           
066000     MOVE SPAR-TISEGKEY(3:6)   TO W26-2-TIM-INV                           
066100     MOVE BE11-CLAG-KDERS      TO W26-2-KDERS                             
066200     MOVE BEN-ENG              TO W26-2-BEART-ENG                         
066300     MOVE BE11-CLAG-PRARTSTD   TO W26-2-PRARTSTD                          
066400     MOVE BE11-CLAG-IDLKTO     TO W26-2-IDLKTO                            
066500                                                                          
066600     WRITE W5132D1-W5132602 FROM W26-2-W5132602                           
066700     MOVE 'W5132D1'            TO POSTSUM-FDNAMN                          
066800     MOVE 'W51322D3'           TO POSTSUM-DDNAMN2                         
066900     MOVE ' 1 '                TO POSTSUM-TRANSTYP                        
067000     CALL POSTSUM USING POSTSUM-PARM                                      
067100     .                                                                    
067200     EJECT                                                                
067300 BBA4-LISTTRANS-JUSTINFO-CDC SECTION.                                     
067400*  SKAPAR LISTPOSTER TILL JUSTERINGSLISTA CDC                             
067500                                                                          
067600     MOVE WS-IDDC            TO JUST-IDDC                                 
067700     MOVE BE01-ART-IDARTNR   TO JUST-IDARTNR                              
067800     MOVE BE11-CLAG-IDANSK   TO JUST-IDANSKNR                             
067900     MOVE BE01-ART-IDLEVNR   TO JUST-IDLEVNR                              
068000     MOVE BE11-CLAG-ADLAGOMR TO JUST-ADLAGOMR                             
068100     MOVE BE01-ART-KDPRODSL  TO JUST-KDPRODSL                             
068200     MOVE BE11-CLAG-KVLS     TO JUST-KVLS                                 
068300     MOVE SB02-INV-KVJUSTKV  TO JUST-KVJUSTKV                             
068400     MOVE BEN-ENG            TO JUST-BEART                                
068500                                                                          
068600     COMPUTE JUST-SUARTSTD-JUST = BE11-CLAG-PRARTSTD *                    
068700                                        SB02-INV-KVJUSTKV                 
068800     MOVE JUST-SUARTSTD-JUST TO JUST-SORTVAERDE                           
068900     IF JUST-SORTVAERDE < ZERO                                            
069000       COMPUTE JUST-SORTVAERDE = JUST-SORTVAERDE * -1                     
069100     END-IF                                                               
069200     MOVE SB02-INV-KDINVKAT-OLD  TO JUST-KDINVKAT                         
069300                                                                          
069400     PERFORM S31-LISTA-JUSTTRANSAR-CDC                                    
069500     .                                                                    
069600     EJECT                                                                
069700 BBB-NDC-SCHEDULE SECTION.                                                
069800                                                                          
069900     EVALUATE TRUE                                                        
070000     WHEN SB02-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                  
070100       IF SB02-INV-FLINVBEH = JA                                          
070200         PERFORM S07-SKRIV-RENSFIL                                        
070300       END-IF                                                             
070400                                                                          
070500     WHEN SB02-INV-KDINVKAT = +2                                          
070600       IF SB02-INV-FLINV4N  = JA AND                                      
070700          SB02-INV-FLINVBEH = JA                                          
070800         PERFORM BBB1-TRANS-UPPD-UTRSALDO-NDC                             
070900         PERFORM S07-SKRIV-RENSFIL                                        
071000       END-IF                                                             
071100                                                                          
071200     WHEN SB02-INV-KDINVKAT = +6                                          
071300       IF SB02-INV-FLINV4R = JA                                           
071400         PERFORM BBB2-LISTTRANS-LAB-INVJUST                               
071500       END-IF                                                             
071600       IF SB02-INV-FLINV4P = JA                                           
071700         PERFORM BBB3-SKAPA-LISTTRANS-JUSTINFO                            
071800       END-IF                                                             
071900       IF SB02-INV-FLINV4R = JA AND                                       
072000          SB02-INV-FLINV4P = JA                                           
072100         PERFORM S07-SKRIV-RENSFIL                                        
072200       END-IF                                                             
072300                                                                          
072400     WHEN SB02-INV-KDINVKAT = +8                                          
072500       IF SB02-INV-FLINV4N = JA                                           
072600         PERFORM BBB1-TRANS-UPPD-UTRSALDO-NDC                             
072700       END-IF                                                             
072800       IF SB02-INV-FLINV4R = JA                                           
072900         PERFORM BBB2-LISTTRANS-LAB-INVJUST                               
073000       END-IF                                                             
073100       IF SB02-INV-FLINV4N = JA AND                                       
073200          SB02-INV-FLINV4R = JA                                           
073300         PERFORM S07-SKRIV-RENSFIL                                        
073400       END-IF                                                             
073500                                                                          
073600     WHEN SB02-INV-KDINVKAT = +11                                         
073700       IF SB02-INV-FLINV4N = JA                                           
073800         PERFORM BBB1-TRANS-UPPD-UTRSALDO-NDC                             
073900         PERFORM S07-SKRIV-RENSFIL                                        
074000       END-IF                                                             
074100                                                                          
074200     WHEN SB02-INV-KDINVKAT = +12                                         
074300       IF SB02-INV-FLINV4R = JA                                           
074400         PERFORM BBB2-LISTTRANS-LAB-INVJUST                               
074500       END-IF                                                             
074600       IF SB02-INV-FLINV4P = JA                                           
074700         PERFORM BBB3-SKAPA-LISTTRANS-JUSTINFO                            
074800       END-IF                                                             
074900       IF SB02-INV-FLINV4R = JA AND                                       
075000          SB02-INV-FLINV4P = JA                                           
075100         PERFORM S07-SKRIV-RENSFIL                                        
075200       END-IF                                                             
075300     END-EVALUATE                                                         
075400     .                                                                    
075500     EJECT                                                                
075600 BBB1-TRANS-UPPD-UTRSALDO-NDC SECTION.                                    
075700                                                                          
075800*  SKAPAR LISTPOSTER TILL LISTA UTREDNINGSSALDO.                          
075900                                                                          
076000     MOVE SPACE                TO L3-NDC-W51322N                          
076100     MOVE +3                   TO L3-NDC-IDLISTA                          
076200     MOVE SB02-INV-KDINVKAT    TO L3-NDC-KDINVKAT                         
076300     MOVE BE01-ART-IDARTNR     TO L3-NDC-IDARTNR                          
076400     MOVE BE11-CLAG-KDPSLLOC   TO L3-NDC-KDPSLLOC                         
076500     MOVE UT-ADARTADR          TO L3-NDC-ADARTADR                         
076600     MOVE WS-IDDC              TO L3-NDC-IDDC                             
076700     MOVE ZERO                 TO L3-NDC-TIAVIDAT                         
076800                                  L3-NDC-KVAVIS                           
076900     MOVE SE11-SLAG-KVAKS-SDC  TO L3-NDC-KVAKS                            
077000     MOVE SE11-SLAG-KVLS       TO L3-NDC-KVLS                             
077100     MOVE SE11-SLAG-KVEFRS     TO L3-NDC-KVEFRS                           
077200     MOVE SE11-SLAG-KVUTRS     TO L3-NDC-KVUTRS                           
077300     MOVE BEN-ENG              TO L3-NDC-BEART                            
077400                                                                          
077500     MOVE SB02-INV-KVJUSTKV    TO L3-NDC-KVJUSTKV                         
077600     MOVE SB02-INV-TISEGKEY    TO SPAR-TISEGKEY                           
077700     MOVE SPAR-TISEGKEY(3:6)   TO L3-NDC-TIJUSTDA                         
077800                                                                          
077900     PERFORM S09-LISTA-UPD-UTRSALDO-NDC                                   
078000     .                                                                    
078100     EJECT                                                                
078200 BBB2-LISTTRANS-LAB-INVJUST SECTION.                                      
078300                                                                          
078400     MOVE +2                 TO W42R1-IDLISTA                             
078500     MOVE WS-IDDC            TO W42R1-IDDC                                
078600     MOVE SE01-SART-IDARTNR  TO W42R1-IDARTNR                             
078700     MOVE SE11-SLAG-ADLAGOMR TO W42R1-ADLAGOMR                            
078800     MOVE BEN-ENG            TO W42R1-BEART-ENG                           
078900     MOVE SE11-SLAG-PRAVCOST TO W42R1-PRAVCOST                            
079000     MOVE BE11-CLAG-KDERS    TO W42R1-KDERS                               
079100     MOVE SB02-INV-KVJUSTKV  TO W42R1-KVJUSTKV                            
079200     MOVE SB02-INV-TISEGKEY  TO SPAR-TISEGKEY                             
079300     MOVE SPAR-TISEGKEY(3:6) TO W42R1-TIM-INV                             
079400     MOVE BE11-CLAG-IDANSK   TO W42R1-IDANSK                              
079500     MOVE SE11-SLAG-KVLS     TO W42R1-KVLS                                
079600                                                                          
079700     COMPUTE W42R1-JUST-VAERDE =                                          
079800               SE11-SLAG-PRAVCOST * SB02-INV-KVJUSTKV                     
079900                                                                          
080000     WRITE W5134R-01 FROM W42R1-W51322R1                                  
080100     MOVE 'W5134R'     TO POSTSUM-FDNAMN                                  
080200     MOVE 'W51322D8'   TO POSTSUM-DDNAMN2                                 
080300     MOVE ' 2 '        TO POSTSUM-TRANSTYP                                
080400     CALL POSTSUM USING POSTSUM-PARM                                      
080500     .                                                                    
080600     EJECT                                                                
080700 BBB3-SKAPA-LISTTRANS-JUSTINFO SECTION.                                   
080800                                                                          
080900*  SKAPAR LISTPOSTER TILL JUSTERINGSLISTA                                 
081000                                                                          
081100     MOVE SPACE              TO L9-NDC-W51322P                            
081200     MOVE +9                 TO L9-NDC-IDLISTA                            
081300     MOVE SE01-SART-IDARTNR  TO L9-NDC-IDARTNR                            
081400     MOVE SE11-SLAG-IDLEVNR  TO L9-NDC-IDLEVNR                            
081500     MOVE BE01-ART-IDFTG     TO L9-NDC-IDFTG                              
081600     MOVE BE11-CLAG-KDPSLLOC TO L9-NDC-KDPSLLOC                           
081700     MOVE BE01-ART-KDSORT    TO L9-NDC-KDSORT                             
081800     MOVE BEN-ENG            TO L9-NDC-BEART                              
081900     MOVE SE11-SLAG-IDPERSON-BUY                                          
082000                             TO L9-NDC-SORTDEL1                           
082100                                L9-NDC-IDANSKNR                           
082200     MOVE WS-IDDC            TO L9-NDC-IDDC                               
082300     MOVE SE11-SLAG-ADLAGOMR TO L9-NDC-ADLAGOMR                           
082400     MOVE ZERO               TO L9-NDC-KVBR                               
082500     MOVE SE11-SLAG-KVLS     TO L9-NDC-KVLS                               
082600     MOVE SB02-INV-KVJUSTKV  TO L9-NDC-KVJUSTKV                           
082700     COMPUTE L9-NDC-JUST-VAERDE = SE11-SLAG-PRAVCOST *                    
082800     SB02-INV-KVJUSTKV                                                    
082900     MOVE L9-NDC-JUST-VAERDE TO L9-NDC-SORTVAERDE                         
083000                                                                          
083100     IF L9-NDC-SORTVAERDE < ZERO                                          
083200       COMPUTE L9-NDC-SORTVAERDE = L9-NDC-SORTVAERDE * (-1)               
083300     END-IF                                                               
083400                                                                          
083500     COMPUTE L9-NDC-SORTVAERDE = MAX-SORTVAERDE -                         
083600                                  L9-NDC-SORTVAERDE                       
083700                                                                          
083800     PERFORM S14-SKRIV-JUSTINFO-NDC                                       
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 BBC-SDC-SCHEDULE SECTION.                                                
084300     EVALUATE TRUE                                                        
084400                                                                          
084500     WHEN SB02-INV-KDINVKAT = +1 OR +3 OR +4 OR +5 OR +9                  
084600       IF SB02-INV-FLINVBEH = JA                                          
084700         PERFORM S07-SKRIV-RENSFIL                                        
084800       END-IF                                                             
084900                                                                          
085000     WHEN SB02-INV-KDINVKAT = +2                                          
085100       IF DCS-NDC-NA                                                      
085200         IF SB02-INV-FLINV4N  = JA AND                                    
085300            SB02-INV-FLINVBEH = JA                                        
085400           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
085500           PERFORM S07-SKRIV-RENSFIL                                      
085600         END-IF                                                           
085700       ELSE                                                               
085800         IF SB02-INV-FLINV2B = JA AND                                     
085900            SB02-INV-FLINVBEH = JA                                        
086000           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
086100           PERFORM S07-SKRIV-RENSFIL                                      
086200         END-IF                                                           
086300       END-IF                                                             
086400                                                                          
086500     WHEN SB02-INV-KDINVKAT = +6                                          
086600       IF DCS-NDC-NA                                                      
086700         IF SB02-INV-FLINV4R = JA                                         
086800           PERFORM BBC4-LISTTRANS-JUST-SDC-PAC                            
086900         END-IF                                                           
087000         IF SB02-INV-FLINV4P = JA                                         
087100           PERFORM BBC3-KVANTJUST-TRANS                                   
087200         END-IF                                                           
087300         IF SB02-INV-FLINV4R = JA AND                                     
087400            SB02-INV-FLINV4P = JA                                         
087500           PERFORM S07-SKRIV-RENSFIL                                      
087600         END-IF                                                           
087700       ELSE                                                               
087800         IF SB02-INV-FLINV2C = JA                                         
087900           PERFORM BBC4-LISTTRANS-JUST-SDC-PAC                            
088000         END-IF                                                           
088100         IF SB02-INV-FLINV2D = JA                                         
088200           PERFORM BBC3-KVANTJUST-TRANS                                   
088300         END-IF                                                           
088400         IF SB02-INV-FLINV2C = JA AND                                     
088500            SB02-INV-FLINV2D = JA                                         
088600           PERFORM S07-SKRIV-RENSFIL                                      
088700         END-IF                                                           
088800       END-IF                                                             
088900                                                                          
089000     WHEN SB02-INV-KDINVKAT = +8                                          
089100       IF DCS-NDC-NA                                                      
089200         IF SB02-INV-FLINV4N = JA                                         
089300           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
089400         END-IF                                                           
089500         IF SB02-INV-FLINV4R = JA                                         
089600           PERFORM BBC2-VARDEJUST-TRANS                                   
089700         END-IF                                                           
089800         IF SB02-INV-FLINV4N = JA AND                                     
089900            SB02-INV-FLINV4R = JA                                         
090000           PERFORM S07-SKRIV-RENSFIL                                      
090100         END-IF                                                           
090200       ELSE                                                               
090300         IF SB02-INV-FLINV2B = JA                                         
090400           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
090500         END-IF                                                           
090600         IF SB02-INV-FLINV2D = JA                                         
090700           PERFORM BBC2-VARDEJUST-TRANS                                   
090800         END-IF                                                           
090900         IF SB02-INV-FLINV2B = JA AND                                     
091000            SB02-INV-FLINV2D = JA                                         
091100           PERFORM S07-SKRIV-RENSFIL                                      
091200         END-IF                                                           
091300       END-IF                                                             
091400                                                                          
091500     WHEN SB02-INV-KDINVKAT = +11                                         
091600       IF DCS-NDC-NA                                                      
091700         IF SB02-INV-FLINV4N = JA                                         
091800           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
091900           PERFORM S07-SKRIV-RENSFIL                                      
092000         END-IF                                                           
092100       ELSE                                                               
092200         IF SB02-INV-FLINV2B = JA                                         
092300           PERFORM BBC1-TRANS-UTREDNSALDO-SDC-PAC                         
092400           PERFORM S07-SKRIV-RENSFIL                                      
092500         END-IF                                                           
092600       END-IF                                                             
092700                                                                          
092800     WHEN SB02-INV-KDINVKAT = +12                                         
092900       IF DCS-NDC-NA                                                      
093000         IF SB02-INV-FLINV4R = JA                                         
093100           PERFORM BBC4-LISTTRANS-JUST-SDC-PAC                            
093200         END-IF                                                           
093300         IF SB02-INV-FLINV4P = JA                                         
093400           PERFORM BBC2-VARDEJUST-TRANS                                   
093500         END-IF                                                           
093600         IF SB02-INV-FLINV4R = JA AND                                     
093700            SB02-INV-FLINV4P = JA                                         
093800           PERFORM S07-SKRIV-RENSFIL                                      
093900         END-IF                                                           
094000       ELSE                                                               
094100         IF SB02-INV-FLINV2C = JA                                         
094200           PERFORM BBC4-LISTTRANS-JUST-SDC-PAC                            
094300         END-IF                                                           
094400         IF SB02-INV-FLINV2D = JA                                         
094500           PERFORM BBC2-VARDEJUST-TRANS                                   
094600         END-IF                                                           
094700         IF SB02-INV-FLINV2C = JA AND                                     
094800            SB02-INV-FLINV2D = JA                                         
094900           PERFORM S07-SKRIV-RENSFIL                                      
095000         END-IF                                                           
095100       END-IF                                                             
095200     END-EVALUATE                                                         
095300     .                                                                    
095400     EJECT                                                                
095500 BBC1-TRANS-UTREDNSALDO-SDC-PAC SECTION.                                  
095600                                                                          
095700*  SKAPAR LISTPOSTER TILL SDC/LDC/NDC-PACIFIC, UPPD. UTREDN.SALDO         
095800                                                                          
095900     MOVE WS-IDDC              TO FYS-IDDC                                
096000     MOVE BE01-ART-IDARTNR     TO FYS-IDARTNR                             
096100     MOVE 0                    TO FYS-KDSORT1                             
096200     MOVE SB02-INV-KDINVKAT    TO FYS-KDINVKAT                            
096300     MOVE UT-ADARTADR          TO FYS-ADARTADR                            
096400     MOVE BE01-ART-KDPRODSL    TO FYS-KDPRODSL                            
096500     MOVE ZERO                 TO FYS-TIAVIDAT                            
096600                                  FYS-KVAVIS                              
096700     MOVE SE11-SLAG-KVAKS-SDC  TO FYS-KVAKS                               
096800     MOVE SE11-SLAG-KVLS       TO FYS-KVLS                                
096900     MOVE SE11-SLAG-KVEFRS     TO FYS-KVEFRS                              
097000     MOVE SE11-SLAG-KVUTRS     TO FYS-KVUTRS                              
097100     MOVE BEN-ENG              TO FYS-BEART                               
097200                                                                          
097300     IF INVJUST-FINNS = JA                                                
097400       MOVE INVH-KVJUSTKV      TO FYS-KVJUSTKV                            
097500       MOVE INVH-DAREGDAT-CLO  TO W-SSAAMMDD                              
097600       MOVE W-AAMMDD           TO FYS-TIJUSTDA                            
097700     ELSE                                                                 
097800       MOVE +0                 TO FYS-KVJUSTKV                            
097900                                  FYS-TIJUSTDA                            
098000     END-IF                                                               
098100                                                                          
098200     IF SDC                                                               
098300     OR (NDC-NA AND DCS-FLWEBDC = JA)                                     
098400       PERFORM S32-LISTA-UPD-UTRSALDO-SDC                                 
098500     ELSE                                                                 
098600       IF NDC-PACIFIC                                                     
098700         PERFORM S34-LISTA-UPD-UTRSALDO-PACIFIC                           
098800       END-IF                                                             
098900     END-IF                                                               
099000     .                                                                    
099100     EJECT                                                                
099200 BBC2-VARDEJUST-TRANS SECTION.                                            
099300                                                                          
099400     MOVE +2                   TO W26-1-IDLISTA                           
099500     MOVE WS-IDDC              TO W26-1-IDDC                              
099600     MOVE SE01-SART-IDARTNR    TO W26-1-IDARTNR                           
099700     MOVE SE11-SLAG-ADLAGOMR   TO W26-1-ADLAGOMR                          
099800     MOVE BEN-ENG              TO W26-1-BEART-ENG                         
099900     MOVE BE11-CLAG-PRARTSTD   TO W26-1-PRARTSTD                          
100000     MOVE BE11-CLAG-KDERS      TO W26-1-KDERS                             
100100     MOVE SB02-INV-KVJUSTKV    TO W26-1-KVJUSTKV                          
100200     MOVE SB02-INV-TISEGKEY    TO SPAR-TISEGKEY                           
100300     MOVE SPAR-TISEGKEY(3:6)   TO W26-1-TIM-INV                           
100400     MOVE BE11-CLAG-IDANSK     TO W26-1-IDANSK                            
100500     MOVE SE11-SLAG-KVLS       TO W26-1-KVLS                              
100600                                                                          
100700     MOVE BE01-ART-KDPRODSL    TO W26-1-KDPRODSL                          
100800     COMPUTE W26-1-JUST-VAERDE =                                          
100900               BE11-CLAG-PRARTSTD * SB02-INV-KVJUSTKV                     
101000                                                                          
101100     WRITE W5132D1-W5132601 FROM W26-1-W5132601                           
101200     MOVE 'W5132D1'            TO POSTSUM-FDNAMN                          
101300     MOVE 'W51322D3'           TO POSTSUM-DDNAMN2                         
101400     MOVE ' 2 '                TO POSTSUM-TRANSTYP                        
101500     CALL POSTSUM USING POSTSUM-PARM                                      
101600     .                                                                    
101700     EJECT                                                                
101800 BBC3-KVANTJUST-TRANS SECTION.                                            
101900                                                                          
102000     MOVE +1                   TO W26-2-IDLISTA                           
102100     MOVE WS-IDDC              TO W26-2-IDDC                              
102200     MOVE SE01-SART-IDARTNR    TO W26-2-IDARTNR                           
102300     MOVE SPACE                TO W26-2-SORT-BLANK                        
102400     MOVE SB02-INV-KVJUSTKV    TO W26-2-KVJUSTKV                          
102500     MOVE SB02-INV-TISEGKEY    TO SPAR-TISEGKEY                           
102600     MOVE SPAR-TISEGKEY(3:6)   TO W26-2-TIM-INV                           
102700     MOVE BE11-CLAG-KDERS      TO W26-2-KDERS                             
102800     MOVE BEN-ENG              TO W26-2-BEART-ENG                         
102900     MOVE BE11-CLAG-PRARTSTD   TO W26-2-PRARTSTD                          
103000     MOVE BE11-CLAG-IDLKTO     TO W26-2-IDLKTO                            
103100                                                                          
103200     WRITE W5132D1-W5132602 FROM W26-2-W5132602                           
103300     MOVE 'W5132D1'            TO POSTSUM-FDNAMN                          
103400     MOVE 'W51322D3'           TO POSTSUM-DDNAMN2                         
103500     MOVE ' 1 '                TO POSTSUM-TRANSTYP                        
103600     CALL POSTSUM USING POSTSUM-PARM                                      
103700     .                                                                    
103800     EJECT                                                                
103900 BBC4-LISTTRANS-JUST-SDC-PAC SECTION.                                     
104000*  SKAPAR LISTPOSTER TILL JUSTERINGSLISTA SDC/LDC/NDC-PACIFIC             
104100                                                                          
104200     MOVE WS-IDDC            TO JUST-IDDC                                 
104300     MOVE SE01-SART-IDARTNR  TO JUST-IDARTNR                              
104400     MOVE BE11-CLAG-IDANSK   TO JUST-IDANSKNR                             
104500     MOVE BE01-ART-IDLEVNR   TO JUST-IDLEVNR                              
104600     MOVE SE11-SLAG-ADLAGOMR TO JUST-ADLAGOMR                             
104700     MOVE BE01-ART-KDPRODSL  TO JUST-KDPRODSL                             
104800     MOVE SE11-SLAG-KVLS     TO JUST-KVLS                                 
104900     MOVE SB02-INV-KVJUSTKV  TO JUST-KVJUSTKV                             
105000     MOVE BEN-ENG            TO JUST-BEART                                
105100                                                                          
105200     COMPUTE JUST-SUARTSTD-JUST = BE11-CLAG-PRARTSTD *                    
105300                                        SB02-INV-KVJUSTKV                 
105400     MOVE JUST-SUARTSTD-JUST TO JUST-SORTVAERDE                           
105500     IF JUST-SORTVAERDE < ZERO                                            
105600       COMPUTE JUST-SORTVAERDE = JUST-SORTVAERDE * -1                     
105700     END-IF                                                               
105800     MOVE SB02-INV-KDINVKAT-OLD TO JUST-KDINVKAT                          
105900                                                                          
106000     IF DCS-SDC OR DCS-NDC-CN                                             
106100     OR (DCS-NDC-PF AND DCS-FLWEBDC = JA)                                 
106200     OR (DCS-NDC-NA AND DCS-FLWEBDC = JA)                                 
106210     OR (DCS-NDC-OTHERS AND DCS-FLWEBDC = JA)                             
106211     OR (DCS-NDC-SA AND DCS-FLWEBDC = JA)                                 
106220      IF DCS-NDC-CN OR DCS-NDC-PF OR DCS-EMIRATES                         
106230       PERFORM S36-LISTA-JUSTTRANSAR-PAC                                  
106240      ELSE                                                                
106300       PERFORM S33-LISTA-JUSTTRANSAR-SDC                                  
106310      END-IF                                                              
106400     ELSE                                                                 
106500       PERFORM S35-LISTA-JUSTTRANSAR-PACIFIC                              
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900 C-AVSLUTNING SECTION.                                                    
107000                                                                          
107100     CLOSE W5132D1                                                        
107200           W51330                                                         
107300           W51331                                                         
107400           W51332                                                         
107500           W51333                                                         
107600           W51334                                                         
107700           W51335                                                         
107710           W51336                                                         
107800           W5134N                                                         
107900           W5134P                                                         
108000           W5134R                                                         
108100           W5134S                                                         
108200           W51385                                                         
108210           W51337                                                         
108300                                                                          
108400     MOVE 'S' TO POSTSUM-OPKOD                                            
108500     CALL POSTSUM USING POSTSUM-PARM                                      
108600     .                                                                    
108700     EJECT                                                                
108800 S01-LAES-ARTREG-WDK6-CDC SECTION.                                        
108900*                            *** LÄSER ARTREG OCH SPARAR UNDAN.           
109000                                                                          
109100     PERFORM IMS-GET-ARTIKEL                                              
109200                                                                          
109300     IF SEGMENT-FINNS                                                     
109400       MOVE BE01-ART-KDERS-UTG TO W-KDERS-UTG                             
109410       MOVE BE01-ART-KDPRODSL  TO W-KDPRODSL-SF                           
109420       MOVE BE01-ART-KDSORT    TO W-KDSORT-SF                             
109430       MOVE BE01-ART-IDLEVNR   TO W-IDLEVNR-SF                            
109500                                                                          
109600       PERFORM IMS-GET-CLAGERINFO                                         
109700                                                                          
109800       IF SEGMENT-FINNS                                                   
109810         MOVE BE11-CLAG-IDANSK     TO W-IDANSK-SF                         
109820         MOVE BE11-CLAG-IDLKTO     TO W-IDLKTO-SF                         
109830         MOVE BE11-CLAG-KDERS      TO W-KDERS-SF                          
109840         MOVE BE11-CLAG-KVAKS-CDC  TO W-KVAKS-CDC-SF                      
109850         MOVE BE11-CLAG-KVAKS-T    TO W-KVAKS-T-SF                        
109860         MOVE BE11-CLAG-PRARTSTD   TO W-PRARTSTD-SF                       
109880         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
109890         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
109891         MOVE BE11-CLAG-KVAKS-PAV  TO W-KVAKS-PAV-SF                      
109892         MOVE BE11-CLAG-KVEFRS     TO W-KVEFRS-SF                         
109893         MOVE BE11-CLAG-KVLS       TO W-KVLS-SF                           
109894         MOVE BE11-CLAG-KVUTRS     TO W-KVUTRS-SF                         
109895         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-1-SF                     
109896         MOVE BE11-CLAG-ADGANG     TO W-ADGANG-1-SF                       
109897         MOVE BE11-CLAG-ADPLATS    TO W-ADPLATS-1-SF                      
109898         MOVE W-ADARTADR-1-SF      TO W-ADARTADR-SF                       
109910         IF BE11-CLAG-PRARTSTD > +0                                       
110000           MOVE JA TO ARTSEGM-FINNS                                       
110100                                                                          
110200           PERFORM IMS-GHU-INVHIST-ROT                                    
110300           IF SEGMENT-FINNS                                               
110400             PERFORM IMS-GET-INVHIST-SEGM                                 
110500             IF SEGMENT-FINNS                                             
110600               MOVE JA TO INVJUST-FINNS                                   
110610               MOVE INVH-FLAUTLSJ   TO W-FLAUTLSJ-SF                      
110620               MOVE INVH-KDJUSTYP   TO W-KDJUSTYP-SF                      
110630               MOVE INVH-DAREGDAT-CLO TO W-TIJUSTDA-SF                    
110700             END-IF                                                       
110800           END-IF                                                         
110900                                                                          
111000           IF W-KDERS-UTG = +0                                            
111100             MOVE BE11-CLAG-ADLAGOMR TO W-ADLAGOMR                        
111200             MOVE BE11-CLAG-ADGANG TO W-ADGANG                            
111300             MOVE BE11-CLAG-ADPLATS TO W-ADPLATS                          
111400           END-IF                                                         
111500         ELSE                                                             
111600           IF SB02-INV-KDINVKAT NOT = +3                                  
111700             PERFORM S07-SKRIV-RENSFIL                                    
111800           END-IF                                                         
111900         END-IF                                                           
112000       END-IF                                                             
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 S02-LAES-ARTREG-WDK7-NDC SECTION.                                        
112500*                            *** LÄSER ARTREG OCH SPARAR UNDAN.           
112600                                                                          
112700     MOVE NEJ                  TO ARTWDK7-FINNS                           
112800     PERFORM IMS-GET-ARTIKEL                                              
112900                                                                          
113000     IF SEGMENT-FINNS                                                     
113100       MOVE BE01-ART-KDERS-UTG TO W-KDERS-UTG                             
113110       MOVE BE01-ART-KDPRODSL  TO W-KDPRODSL-SF                           
113120       MOVE BE01-ART-KDSORT    TO W-KDSORT-SF                             
113200                                                                          
113300       PERFORM IMS-GET-CLAGERINFO                                         
113400                                                                          
113500       IF SEGMENT-FINNS                                                   
113510         MOVE BE11-CLAG-IDANSK     TO W-IDANSK-SF                         
113520         MOVE BE11-CLAG-IDLKTO     TO W-IDLKTO-SF                         
113530         MOVE BE11-CLAG-KDERS      TO W-KDERS-SF                          
113540         MOVE BE11-CLAG-KVAKS-CDC  TO W-KVAKS-CDC-SF                      
113550         MOVE BE11-CLAG-KVAKS-T    TO W-KVAKS-T-SF                        
113560         MOVE BE11-CLAG-PRARTSTD   TO W-PRARTSTD-SF                       
113570         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
113580         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
113590         MOVE BE11-CLAG-KVAKS-PAV  TO W-KVAKS-PAV-SF                      
113591         MOVE BE11-CLAG-KVEFRS     TO W-KVEFRS-SF                         
113592         MOVE BE11-CLAG-KVLS       TO W-KVLS-SF                           
113593         MOVE BE11-CLAG-KVUTRS     TO W-KVUTRS-SF                         
113594         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-1-SF                     
113595         MOVE BE11-CLAG-ADGANG     TO W-ADGANG-1-SF                       
113596         MOVE BE11-CLAG-ADPLATS    TO W-ADPLATS-1-SF                      
113597         MOVE W-ADARTADR-1-SF      TO W-ADARTADR-SF                       
113600                                                                          
113700         PERFORM IMS-GU-WDK701                                            
113800         IF SEGMENT-FINNS                                                 
113900           MOVE JA             TO ARTWDK7-FINNS                           
114000           MOVE JA             TO ARTSEGM-FINNS                           
114100           PERFORM IMS-GNP-WDK711                                         
114200                                                                          
114300           IF W-KDERS-UTG = +0                                            
114400             MOVE SE11-SLAG-ADLAGOMR TO W-ADLAGOMR                        
114500             MOVE SE11-SLAG-ADGANG   TO W-ADGANG                          
114600             MOVE SE11-SLAG-ADPLATS  TO W-ADPLATS                         
114700           END-IF                                                         
114710           MOVE SE11-SLAG-PRAVCOST   TO W-PRAVCOST-SF                     
114720           MOVE SE11-SLAG-KVAKS-SDC  TO W-KVAKS-SDC-SF                    
114730           MOVE SE11-SLAG-ADLAGOMR   TO W-ADLAGOMR-SF                     
114740           MOVE SE11-SLAG-KVAKS-PAV  TO W-KVAKS-PAV-SF                    
114750           MOVE SE11-SLAG-KVEFRS     TO W-KVEFRS-SF                       
114760           MOVE SE11-SLAG-KVLS       TO W-KVLS-SF                         
114770           MOVE SE11-SLAG-KVUTRS     TO W-KVUTRS-SF                       
114780           MOVE SE11-SLAG-IDLEVNR    TO W-IDLEVNR-SF                      
114790           MOVE SE11-SLAG-ADLAGOMR   TO W-ADLAGOMR-1-SF                   
114800           MOVE SE11-SLAG-ADGANG     TO W-ADGANG-1-SF                     
114801           MOVE SE11-SLAG-ADPLATS    TO W-ADPLATS-1-SF                    
114802           MOVE W-ADARTADR-1-SF      TO W-ADARTADR-SF                     
114890                                                                          
114900           PERFORM IMS-GHU-INVHIST-ROT                                    
115000           IF SEGMENT-FINNS                                               
115100             PERFORM IMS-GET-INVHIST-SEGM                                 
115200             IF SEGMENT-FINNS                                             
115300               MOVE JA TO INVJUST-FINNS                                   
115310               MOVE INVH-FLAUTLSJ   TO W-FLAUTLSJ-SF                      
115320               MOVE INVH-KDJUSTYP   TO W-KDJUSTYP-SF                      
115330               MOVE INVH-DAREGDAT-CLO TO W-TIJUSTDA-SF                    
115400             END-IF                                                       
115500           END-IF                                                         
115600         END-IF                                                           
115700                                                                          
115800       END-IF                                                             
115900     END-IF                                                               
116000     .                                                                    
116100     EJECT                                                                
116200 S03-LAES-ARTREG-WDK7-SDC SECTION.                                        
116300*                            *** LÄSER ARTREG OCH SPARAR UNDAN.           
116400                                                                          
116500     MOVE NEJ                  TO ARTWDK7-FINNS                           
116600     PERFORM IMS-GET-ARTIKEL                                              
116700     IF SEGMENT-FINNS                                                     
116800       MOVE BE01-ART-KDERS-UTG TO W-KDERS-UTG                             
116810       MOVE BE01-ART-KDPRODSL  TO W-KDPRODSL-SF                           
116820       MOVE BE01-ART-KDSORT    TO W-KDSORT-SF                             
116900                                                                          
117000       PERFORM IMS-GET-CLAGERINFO                                         
117100                                                                          
117200       IF SEGMENT-FINNS                                                   
117210         MOVE BE11-CLAG-IDANSK     TO W-IDANSK-SF                         
117220         MOVE BE11-CLAG-IDLKTO     TO W-IDLKTO-SF                         
117230         MOVE BE11-CLAG-KDERS      TO W-KDERS-SF                          
117240         MOVE BE11-CLAG-KVAKS-CDC  TO W-KVAKS-CDC-SF                      
117250         MOVE BE11-CLAG-KVAKS-T    TO W-KVAKS-T-SF                        
117260         MOVE BE11-CLAG-PRARTSTD   TO W-PRARTSTD-SF                       
117270         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
117280         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-SF                       
117290         MOVE BE11-CLAG-KVAKS-PAV  TO W-KVAKS-PAV-SF                      
117291         MOVE BE11-CLAG-KVEFRS     TO W-KVEFRS-SF                         
117292         MOVE BE11-CLAG-KVLS       TO W-KVLS-SF                           
117293         MOVE BE11-CLAG-KVUTRS     TO W-KVUTRS-SF                         
117294         MOVE BE11-CLAG-ADLAGOMR   TO W-ADLAGOMR-1-SF                     
117295         MOVE BE11-CLAG-ADGANG     TO W-ADGANG-1-SF                       
117296         MOVE BE11-CLAG-ADPLATS    TO W-ADPLATS-1-SF                      
117297         MOVE W-ADARTADR-1-SF      TO W-ADARTADR-SF                       
117300         PERFORM IMS-GU-WDK701                                            
117400         IF SEGMENT-FINNS                                                 
117500           MOVE JA             TO ARTWDK7-FINNS                           
117600           PERFORM IMS-GNP-WDK711                                         
117700                                                                          
117800           IF W-KDERS-UTG = +0                                            
117900             MOVE SE11-SLAG-ADLAGOMR TO W-ADLAGOMR                        
118000             MOVE SE11-SLAG-ADGANG   TO W-ADGANG                          
118100             MOVE SE11-SLAG-ADPLATS  TO W-ADPLATS                         
118200           END-IF                                                         
118300                                                                          
118310           MOVE SE11-SLAG-PRAVCOST   TO W-PRAVCOST-SF                     
118320           MOVE SE11-SLAG-KVAKS-SDC  TO W-KVAKS-SDC-SF                    
118330           MOVE SE11-SLAG-ADLAGOMR   TO W-ADLAGOMR-SF                     
118340           MOVE SE11-SLAG-KVAKS-PAV  TO W-KVAKS-PAV-SF                    
118350           MOVE SE11-SLAG-KVEFRS     TO W-KVEFRS-SF                       
118360           MOVE SE11-SLAG-KVLS       TO W-KVLS-SF                         
118370           MOVE SE11-SLAG-KVUTRS     TO W-KVUTRS-SF                       
118380           MOVE SE11-SLAG-IDLEVNR    TO W-IDLEVNR-SF                      
118381           MOVE SE11-SLAG-ADLAGOMR   TO W-ADLAGOMR-1-SF                   
118382           MOVE SE11-SLAG-ADGANG     TO W-ADGANG-1-SF                     
118383           MOVE SE11-SLAG-ADPLATS    TO W-ADPLATS-1-SF                    
118384           MOVE W-ADARTADR-1-SF      TO W-ADARTADR-SF                     
118398                                                                          
118400           PERFORM IMS-GHU-INVHIST-ROT                                    
118500           IF SEGMENT-FINNS                                               
118600             PERFORM IMS-GET-INVHIST-SEGM                                 
118700             IF SEGMENT-FINNS                                             
118800               MOVE JA TO INVJUST-FINNS                                   
118810               MOVE INVH-FLAUTLSJ   TO W-FLAUTLSJ-SF                      
118820               MOVE INVH-KDJUSTYP   TO W-KDJUSTYP-SF                      
118830               MOVE INVH-DAREGDAT-CLO TO W-TIJUSTDA-SF                    
118900             END-IF                                                       
119000           END-IF                                                         
119100         END-IF                                                           
119200                                                                          
119300         IF BE11-CLAG-PRARTSTD > +0                                       
119400           IF ARTWDK7-FINNS = JA                                          
119500             MOVE JA TO ARTSEGM-FINNS                                     
119600           END-IF                                                         
119700         ELSE                                                             
119800           IF SB02-INV-KDINVKAT NOT = +3                                  
119900             PERFORM S07-SKRIV-RENSFIL                                    
120000           END-IF                                                         
120100         END-IF                                                           
120200       END-IF                                                             
120300     END-IF                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 S05-LAES-BENAEMN  SECTION.                                               
120700*  LÄS BENÄMNING                                                          
120800                                                                          
120900     MOVE 'S  '            TO W-IDSKYLT                                   
121000     PERFORM IMS-GU-BEN                                                   
121100     IF SEGMENT-FINNS                                                     
121200       MOVE BEN-TEXT-BEART TO BEN-SV                                      
121300     ELSE                                                                 
121400       MOVE SPACE          TO BEN-SV                                      
121500     END-IF                                                               
121600                                                                          
121700     MOVE 'GB '            TO W-IDSKYLT                                   
121800     PERFORM IMS-GU-BEN                                                   
121900     IF SEGMENT-FINNS                                                     
122000       MOVE BEN-TEXT-BEART TO BEN-ENG                                     
122010       MOVE BEN-TEXT-BEART TO W-BEART-SF                                  
122100     ELSE                                                                 
122200       MOVE SPACE          TO BEN-ENG                                     
122300     END-IF                                                               
122400     .                                                                    
122500     EJECT                                                                
122600 S07-SKRIV-RENSFIL  SECTION.                                              
122700                                                                          
122800     MOVE 'D'               TO W42S-KDBEH                                 
122900     MOVE ART-IDARTNR       TO W42S-IDARTNR                               
123000     MOVE SB02-INV-IDDC     TO W42S-IDDC                                  
123100     MOVE SB02-INV-KDINVKAT TO W42S-KDINVKAT                              
123200     MOVE SB02-INV-TISEGKEY TO W42S-TISEGKEY                              
123300     MOVE SB02-INV-KVJUSTKV TO W42S-KVJUSTKV                              
123400                                                                          
123500     WRITE W5134S-UT   FROM W42S-W51322S                                  
123600     MOVE 'W5134S' TO POSTSUM-FDNAMN                                      
123700     MOVE 'W51322D9' TO POSTSUM-DDNAMN2                                   
123800     MOVE '   ' TO POSTSUM-TRANSTYP                                       
123900     CALL POSTSUM USING POSTSUM-PARM                                      
124000     .                                                                    
124100     SKIP3                                                                
124200 S09-LISTA-UPD-UTRSALDO-NDC SECTION.                                      
124300                                                                          
124400     WRITE W5134N-003  FROM L3-NDC-W51322N                                
124500     MOVE 'W5134N' TO POSTSUM-FDNAMN                                      
124600     MOVE 'W51322D6' TO POSTSUM-DDNAMN2                                   
124700     MOVE '   ' TO POSTSUM-TRANSTYP                                       
124800     CALL POSTSUM USING POSTSUM-PARM                                      
124900     .                                                                    
125000     SKIP3                                                                
125100 S14-SKRIV-JUSTINFO-NDC SECTION.                                          
125200                                                                          
125300     WRITE W5134P-009  FROM L9-NDC-W51322P                                
125400     MOVE 'W5134P' TO POSTSUM-FDNAMN                                      
125500     MOVE 'W51322D7' TO POSTSUM-DDNAMN2                                   
125600     MOVE '   ' TO POSTSUM-TRANSTYP                                       
125700     CALL POSTSUM USING POSTSUM-PARM                                      
125800     .                                                                    
125900     SKIP2                                                                
126000 S30-LISTA-UPD-UTRSALDO-CDC     SECTION.                                  
126100                                                                          
126200     WRITE W51330-POST FROM FYS-AREA                                      
126300     MOVE 'W51330'   TO POSTSUM-FDNAMN                                    
126400     MOVE 'W51322DD' TO POSTSUM-DDNAMN2                                   
126500     MOVE 'U30'      TO POSTSUM-TRANSTYP                                  
126600     CALL POSTSUM USING POSTSUM-PARM                                      
126700     .                                                                    
126800     SKIP3                                                                
126900 S31-LISTA-JUSTTRANSAR-CDC     SECTION.                                   
127000                                                                          
127100     WRITE W51331-POST FROM JUST-AREA                                     
127200     MOVE 'W51331'   TO POSTSUM-FDNAMN                                    
127300     MOVE 'W51322DE' TO POSTSUM-DDNAMN2                                   
127400     MOVE 'U31'      TO POSTSUM-TRANSTYP                                  
127500     CALL POSTSUM USING POSTSUM-PARM                                      
127600     .                                                                    
127700     SKIP3                                                                
127800 S32-LISTA-UPD-UTRSALDO-SDC     SECTION.                                  
127900                                                                          
128000     WRITE W51332-POST FROM FYS-AREA                                      
128100     MOVE 'W51332'   TO POSTSUM-FDNAMN                                    
128200     MOVE 'W51322DF' TO POSTSUM-DDNAMN2                                   
128300     MOVE 'U32'      TO POSTSUM-TRANSTYP                                  
128400     CALL POSTSUM USING POSTSUM-PARM                                      
128500     .                                                                    
128600     SKIP3                                                                
128700 S33-LISTA-JUSTTRANSAR-SDC     SECTION.                                   
128800                                                                          
128900     WRITE W51333-POST FROM JUST-AREA                                     
129000     MOVE 'W51333'   TO POSTSUM-FDNAMN                                    
129100     MOVE 'W51322DG' TO POSTSUM-DDNAMN2                                   
129200     MOVE 'U33'      TO POSTSUM-TRANSTYP                                  
129300     CALL POSTSUM USING POSTSUM-PARM                                      
129400     .                                                                    
129500     SKIP3                                                                
129600 S34-LISTA-UPD-UTRSALDO-PACIFIC SECTION.                                  
129700                                                                          
129800     WRITE W51334-POST FROM FYS-AREA                                      
129900     MOVE 'W51334'   TO POSTSUM-FDNAMN                                    
130000     MOVE 'W51322DH' TO POSTSUM-DDNAMN2                                   
130100     MOVE 'U34'      TO POSTSUM-TRANSTYP                                  
130200     CALL POSTSUM USING POSTSUM-PARM                                      
130300     .                                                                    
130400     SKIP3                                                                
130500 S35-LISTA-JUSTTRANSAR-PACIFIC SECTION.                                   
130600                                                                          
130700     WRITE W51335-POST FROM JUST-AREA                                     
130800     MOVE 'W51335'   TO POSTSUM-FDNAMN                                    
130900     MOVE 'W51322DI' TO POSTSUM-DDNAMN2                                   
131000     MOVE 'U35'      TO POSTSUM-TRANSTYP                                  
131100     CALL POSTSUM USING POSTSUM-PARM                                      
131200     .                                                                    
131300     EJECT                                                                
131310 S36-LISTA-JUSTTRANSAR-PAC     SECTION.                                   
131320                                                                          
131330     WRITE W51336-POST FROM JUST-AREA                                     
131340     MOVE 'W51336'   TO POSTSUM-FDNAMN                                    
131350     MOVE 'W51322DJ' TO POSTSUM-DDNAMN2                                   
131360     MOVE 'U36'      TO POSTSUM-TRANSTYP                                  
131370     CALL POSTSUM USING POSTSUM-PARM                                      
131380     .                                                                    
131390     SKIP3                                                                
131400 S85-SKRIV-ERS-BEVAKNING SECTION.                                         
131500                                                                          
131600     MOVE ART-IDARTNR      TO ERSBEV-IDARTNR                              
131700     MOVE SB02-INV-IDDC    TO ERSBEV-IDDC                                 
131800                                                                          
131900     WRITE ERSBEV-POST FROM ERSBEV-AREA                                   
132000                                                                          
132100     MOVE 'W51385'   TO POSTSUM-FDNAMN                                    
132200     MOVE 'W51322DB' TO POSTSUM-DDNAMN2                                   
132300     MOVE 'ERSBEV'   TO POSTSUM-TRANSTYP                                  
132400     CALL POSTSUM USING POSTSUM-PARM                                      
132500     .                                                                    
132600     EJECT                                                                
132700* - - - - - - - - - - - - - *                                             
132800*    OPERATIONER MOT WDH1   *                                             
132900* - - - - - - - - - - - - - *                                             
133000 IMS-GET-INVENTERINGSROT SECTION.                                         
133100                                                                          
133200     MOVE 'WDH101 ' TO SSA1                                               
133300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
133400     CALL CBLTDLI USING GN WDH1-PCB WDH101 SSA1                           
133500     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800     EJECT                                                                
133900 IMS-GET-INVENTERINGSINF SECTION.                                         
134000                                                                          
134100     STRING  'WDH111  (WDH111KY>=' KEYWDH1-MIN                            
134200                     '&WDH111KY<=' KEYWDH1-MAX ')'                        
134300              DELIMITED BY SIZE INTO SSA1                                 
134400     MOVE '  GE' TO GODK-STATUSKODER                                      
134500     CALL CBLTDLI USING GHNP WDH1-PCB SB02-WDH111 SSA1                    
134600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
134700     PERFORM IMS-STATUSKONTROLL                                           
134800                                                                          
134900     .                                                                    
135000     SKIP2                                                                
135100* - - - - - - - - - - - - - *                                             
135200*    OPERATIONER MOT WDK6   *                                             
135300* - - - - - - - - - - - - - *                                             
135400 IMS-GET-ARTIKEL SECTION.                                                 
135500*                            *** LÄSER AKTUELL ARTIKEL                    
135600*                            *** EJ UTGÅNGNA                              
135700     STRING                                                               
135800     'WLARTC01(IDARTNR  =' ARTNR                                          
135900     '&KDERS    =' W-KDERS-X ')'                                          
136000     DELIMITED BY SIZE INTO SSA1                                          
136100     MOVE '  GE' TO GODK-STATUSKODER                                      
136200     CALL CBLTDLI USING GU WLARTC-PCB BE01-WLARTC01 SSA1                  
136300     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
136400     PERFORM IMS-STATUSKONTROLL                                           
136500     .                                                                    
136600     SKIP2                                                                
136700 IMS-GET-CLAGERINFO SECTION.                                              
136800                                                                          
136900     MOVE 'WLARTC11 ' TO SSA1                                             
137000     MOVE '  GE' TO GODK-STATUSKODER                                      
137100     CALL CBLTDLI USING GNP WLARTC-PCB BE11-WLARTC11 SSA1                 
137200     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
137300     PERFORM IMS-STATUSKONTROLL                                           
137400     .                                                                    
137500     EJECT                                                                
137600* - - - - - - - - - - - - - *                                             
137700*    OPERATIONER MOT WDK7   *                                             
137800* - - - - - - - - - - - - - *                                             
137900                                                                          
138000 IMS-GU-WDK701 SECTION.                                                   
138100                                                                          
138200     STRING 'WLARTS01(IDARTNR  =' ARTNR ')'                               
138300            DELIMITED BY SIZE INTO SSA1                                   
138400     MOVE '  GE' TO GODK-STATUSKODER                                      
138500     CALL CBLTDLI USING GU WLARTS-PCB SE01-WLARTS01 SSA1                  
138600     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
138700     PERFORM IMS-STATUSKONTROLL                                           
138800     .                                                                    
138900     SKIP2                                                                
139000 IMS-GNP-WDK711 SECTION.                                                  
139100                                                                          
139200     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
139300            DELIMITED BY SIZE INTO SSA1                                   
139400     MOVE '  GE' TO GODK-STATUSKODER                                      
139500     CALL CBLTDLI USING GNP WLARTS-PCB SE11-WLARTS11 SSA1                 
139600     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900     EJECT                                                                
140000* - - - - - - - - - - - - - *                                             
140100*    OPERATIONER MOT WDH7   *                                             
140200* - - - - - - - - - - - - - *                                             
140300                                                                          
140400 IMS-GHU-INVHIST-ROT SECTION.                                             
140500                                                                          
140600     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
140700            DELIMITED BY SIZE INTO SSA1                                   
140800     MOVE '  GE' TO GODK-STATUSKODER                                      
140900     CALL CBLTDLI USING GHU WLINVC-PCB WLINVC01 SSA1                      
141000     MOVE WLINVC-STATUS-CODE TO STATUS-WS                                 
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     EJECT                                                                
141400 IMS-GET-INVHIST-SEGM SECTION.                                            
141500                                                                          
141600     STRING 'WLINVC11(TISEGKEY<=' W-TISEGKEY-X                            
141700                    '&IDDC     =' W-IDDC-X ')'                            
141800            DELIMITED BY SIZE INTO SSA1                                   
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP WLINVC-PCB WLINVC11 SSA1                      
142100     MOVE WLINVC-STATUS-CODE TO STATUS-WS                                 
142200     PERFORM IMS-STATUSKONTROLL                                           
142300     .                                                                    
142400     SKIP2                                                                
142500 IMS-GU-BEN  SECTION.                                                     
142600                                                                          
142700     STRING 'WLBENA01(WDD3BSEQ =' ARTNR ')'                               
142800              DELIMITED BY SIZE INTO SSA1                                 
142900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
143000              DELIMITED BY SIZE INTO SSA2                                 
143100     MOVE '  GE' TO GODK-STATUSKODER                                      
143200     CALL CBLTDLI USING GU BEN-PCB BEN-WLBENA11 SSA1 SSA2                 
143300     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
143400     PERFORM IMS-STATUSKONTROLL                                           
143500     .                                                                    
143600     SKIP2                                                                
143700 IMS-GU-WDB601    SECTION.                                                
143800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
143900          DELIMITED BY SIZE INTO SSA1                                     
144000     MOVE '  ' TO GODK-STATUSKODER                                        
144100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
144200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
144300     PERFORM IMS-STATUSKONTROLL                                           
144400     .                                                                    
144500     SKIP2                                                                
144600 IMS-STATUSKONTROLL SECTION.                                              
144700                                                                          
144800     SET STATUS-IX TO 1                                                   
144900     SEARCH GODK-STATUS AT END CALL FELLOG                                
145000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
145100     CONTINUE                                                             
145200     END-SEARCH                                                           
145300     .                                                                    
