000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5123800.                                                
000400*AUTHOR.         ROYNA LUND.                                              
000500*DATE-WRITTEN.   93/04/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        VARULAGERVÄRDERINGEN.                                            
001100*        FÖRST HÄMTAS RANDOMKEY PER IN-TRANS,                             
001200*        SEDAN SORTERAS DENNA FIL PER RANDOMKEY,                          
001300*        DÄREFTER SKER BEHANDLINGEN ARTIKEL FÖR ARTIKEL.                  
001400*        NUVÄRDE BERÄKNAS FRÅN INKÖPSPRIS,HEMTAGNINGSFAKTOR               
001500*        OCH SALDO.                                                       
001600*        ANSKAFFNINGSVÄRDE BERÄKNAS MED HJÄLP AV UTLÄNDSKT                
001700*        BESTÄLLNINGSPRIS INLEVERANSHISTORIK OCH KURSFIL.                 
001800*                                                                         
001900*        PROGRAMMET LÄSER:     WLLEVA (WDF1)                              
002000*                              WLINLE (WDL2)                              
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- KURSFIL                                                    
002900     SELECT KURSFIL                    ASSIGN TO W51238D1.                
003000     SKIP2                                                                
003100*          --- PRIS OCH SALDO INFO FÖR CETRALA EJ UTGÅNGNA ARTIKLA        
003200     SELECT INFIL                      ASSIGN TO W51238D2.                
003300     SKIP2                                                                
003400*          --- NUVÄRDE OCH ANSKAFFNINGSVÄRDE FÖR CENTRALA ARTIKLAW        
003500     SELECT UTFIL1                     ASSIGN TO W51238D3.                
003600     SKIP2                                                                
003700*          --- SOM UTFIL1 MEN EXKL SLATTAR                                
003800     SELECT UTFIL2                     ASSIGN TO W51238D4.                
003900*          --- SORTFIL, SORTERAR PÅ RANDOMKEY                             
004000     SELECT SORTFIL                    ASSIGN TO W51238DS.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  KURSFIL                                                              
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000*01  -COPY W51237      -L.                                                
005100     SKIP3                                                                
005200 FD  INFIL                                                                
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  IN-RECORD -COPY W51233      -L.                                      
005700     SKIP3                                                                
005800 FD  UTFIL1                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200*01  POST -COPY W51239 -PRE  UT1-  -L.                                    
006300     SKIP3                                                                
006400 FD  UTFIL2                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700     SKIP2                                                                
006800*01  POST -COPY W51239 -PRE  UT2-  -L.                                    
006900     EJECT                                                                
007000 SD  SORTFIL.                                                             
007100                                                                          
007200 01  SORT-RECORD.                                                         
007300     03  SORT-RANDOMKEY        PIC X(4).                                  
007400*    03  POST -COPY W51233 -PRE  SORT-.                                   
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700                                                                          
007800*    -COPY WY2000W1                                                       
007900 77  IDPGM                     PIC X(8)    VALUE 'W5123800'.              
008000 77  JA                        PIC X       VALUE 'J'.                     
008100 77  NEJ                       PIC X       VALUE 'N'.                     
008200 77  DATABASE                  PIC X(4)    VALUE 'WDL2'.                  
008300                                                                          
008400 77  KURSFIL-EOF-SW            PIC X       VALUE 'N'.                     
008500 77  INFIL-EOF-SW              PIC X       VALUE 'N'.                     
008600 77  SORTFIL-EOF-SW            PIC X       VALUE 'N'.                     
008700                                                                          
008800*    --- INDEXFÄLT                                                        
008900                                                                          
009000 77  IX                        PIC S9(9)   COMP SYNC   VALUE +0.          
009100 77  MAX-INLEV                 PIC S9(9)   COMP SYNC   VALUE +0.          
009200 77  MAX-KVANT                 PIC S9(9)   COMP SYNC   VALUE +0.          
009300 77  MAX-GODK                  PIC S9(9)   COMP SYNC   VALUE +0.          
009400 77  INLEV-EFTER-KVANT         PIC S9(9)   COMP SYNC   VALUE +0.          
009500                                                                          
009600*    --- ARBETSFÄLT                                                       
009700                                                                          
009800 01  AKT-TIDATUM               PIC 9(6)    VALUE ZERO.                    
009900 01  AKT-DADATUM               PIC 9(8)    VALUE ZERO.                    
010000 01  W-DAAVIDAT                PIC 9(8)    VALUE ZERO.                    
010100 01  W-DAAVSDAT                PIC 9(8)    VALUE ZERO.                    
010200                                                                          
010300 01  W-INLEV-DATUM             PIC 9(8).                                  
010400                                                                          
010500 01  SALDO                     PIC S9(7)   COMP-3    VALUE +0.            
010600                                                                          
010700 01 W-PRKURS                   PIC 9(6)V9(5).                             
010800                                                                          
010900 01 W-SUARTVLV                 PIC S9(11)V9(2).                           
011000     EJECT                                                                
011100 01  FILLER                    PIC X(24)   VALUE 'KURSTABELLER'.          
011200*** LAGRAS I FALLANDE DATUMORDNING                                        
011300                                                                          
011400 01  KURSTABELL-ATS.                                                      
011500     03  ATS           OCCURS 12000  INDEXED BY ATS-IX.                   
011600         05 ATS-DATUM-FOM      PIC 9(8).                                  
011700         05 FILLER             PIC X.                                     
011800         05 ATS-DATUM-TOM      PIC 9(8).                                  
011900         05 FILLER             PIC X.                                     
012000         05 ATS-PRKURS         PIC 9(6)V9(5).                             
012100                                                                          
012200 01  KURSTABELL-AUD.                                                      
012300     03  AUD           OCCURS 12000  INDEXED BY AUD-IX.                   
012400         05 AUD-DATUM-FOM      PIC 9(8).                                  
012500         05 FILLER             PIC X.                                     
012600         05 AUD-DATUM-TOM      PIC 9(8).                                  
012700         05 FILLER             PIC X.                                     
012800         05 AUD-PRKURS         PIC 9(6)V9(5).                             
012900                                                                          
013000 01  KURSTABELL-BEF.                                                      
013100     03  BEF           OCCURS 12000  INDEXED BY BEF-IX.                   
013200         05 BEF-DATUM-FOM      PIC 9(8).                                  
013300         05 FILLER             PIC X.                                     
013400         05 BEF-DATUM-TOM      PIC 9(8).                                  
013500         05 FILLER             PIC X.                                     
013600         05 BEF-PRKURS         PIC 9(6)V9(5).                             
013700     EJECT                                                                
013710 01  KURSTABELL-BRL.                                                      
013720     03  BRL           OCCURS 12000  INDEXED BY BRL-IX.                   
013730         05 BRL-DATUM-FOM      PIC 9(8).                                  
013740         05 FILLER             PIC X.                                     
013750         05 BRL-DATUM-TOM      PIC 9(8).                                  
013760         05 FILLER             PIC X.                                     
013770         05 BRL-PRKURS         PIC 9(6)V9(5).                             
013780                                                                          
013800 01  KURSTABELL-CAD.                                                      
013900     03  CAD           OCCURS 12000  INDEXED BY CAD-IX.                   
014000         05 CAD-DATUM-FOM      PIC 9(8).                                  
014100         05 FILLER             PIC X.                                     
014200         05 CAD-DATUM-TOM      PIC 9(8).                                  
014300         05 FILLER             PIC X.                                     
014400         05 CAD-PRKURS         PIC 9(6)V9(5).                             
014500                                                                          
014600 01  KURSTABELL-CHF.                                                      
014700     03  CHF           OCCURS 12000  INDEXED BY CHF-IX.                   
014800         05 CHF-DATUM-FOM      PIC 9(8).                                  
014900         05 FILLER             PIC X.                                     
015000         05 CHF-DATUM-TOM      PIC 9(8).                                  
015100         05 FILLER             PIC X.                                     
015200         05 CHF-PRKURS         PIC 9(6)V9(5).                             
015300                                                                          
015400 01  KURSTABELL-CLP.                                                      
015500     03  CLP           OCCURS 12000  INDEXED BY CLP-IX.                   
015600         05 CLP-DATUM-FOM      PIC 9(8).                                  
015700         05 FILLER             PIC X.                                     
015800         05 CLP-DATUM-TOM      PIC 9(8).                                  
015900         05 FILLER             PIC X.                                     
016000         05 CLP-PRKURS         PIC 9(6)V9(5).                             
016100                                                                          
016200 01  KURSTABELL-CNY.                                                      
016210     03  CNY           OCCURS 12000  INDEXED BY CNY-IX.                   
016220         05 CNY-DATUM-FOM      PIC 9(8).                                  
016230         05 FILLER             PIC X.                                     
016240         05 CNY-DATUM-TOM      PIC 9(8).                                  
016250         05 FILLER             PIC X.                                     
016260         05 CNY-PRKURS         PIC 9(6)V9(5).                             
016270                                                                          
016300 01  KURSTABELL-CSK.                                                      
016400     03  CSK           OCCURS 12000  INDEXED BY CSK-IX.                   
016500         05 CSK-DATUM-FOM      PIC 9(8).                                  
016600         05 FILLER             PIC X.                                     
016700         05 CSK-DATUM-TOM      PIC 9(8).                                  
016800         05 FILLER             PIC X.                                     
016900         05 CSK-PRKURS         PIC 9(6)V9(5).                             
017000                                                                          
017100 01  KURSTABELL-CZK.                                                      
017200     03  CZK           OCCURS 12000  INDEXED BY CZK-IX.                   
017300         05 CZK-DATUM-FOM      PIC 9(8).                                  
017400         05 FILLER             PIC X.                                     
017500         05 CZK-DATUM-TOM      PIC 9(8).                                  
017600         05 FILLER             PIC X.                                     
017700         05 CZK-PRKURS         PIC 9(6)V9(5).                             
017800                                                                          
017900 01  KURSTABELL-DEM.                                                      
018000     03  DEM           OCCURS 12000  INDEXED BY DEM-IX.                   
018100         05 DEM-DATUM-FOM      PIC 9(8).                                  
018200         05 FILLER             PIC X.                                     
018300         05 DEM-DATUM-TOM      PIC 9(8).                                  
018400         05 FILLER             PIC X.                                     
018500         05 DEM-PRKURS         PIC 9(6)V9(5).                             
018600     EJECT                                                                
018700 01  KURSTABELL-DKK.                                                      
018800     03  DKK           OCCURS 12000  INDEXED BY DKK-IX.                   
018900         05 DKK-DATUM-FOM      PIC 9(8).                                  
019000         05 FILLER             PIC X.                                     
019100         05 DKK-DATUM-TOM      PIC 9(8).                                  
019200         05 FILLER             PIC X.                                     
019300         05 DKK-PRKURS         PIC 9(6)V9(5).                             
019400                                                                          
019500 01  KURSTABELL-ESP.                                                      
019600     03  ESP           OCCURS 12000  INDEXED BY ESP-IX.                   
019700         05 ESP-DATUM-FOM      PIC 9(8).                                  
019800         05 FILLER             PIC X.                                     
019900         05 ESP-DATUM-TOM      PIC 9(8).                                  
020000         05 FILLER             PIC X.                                     
020100         05 ESP-PRKURS         PIC 9(6)V9(5).                             
020200                                                                          
020300 01  KURSTABELL-FIM.                                                      
020400     03  FIM           OCCURS 12000  INDEXED BY FIM-IX.                   
020500         05 FIM-DATUM-FOM      PIC 9(8).                                  
020600         05 FILLER             PIC X.                                     
020700         05 FIM-DATUM-TOM      PIC 9(8).                                  
020800         05 FILLER             PIC X.                                     
020900         05 FIM-PRKURS         PIC 9(6)V9(5).                             
021000                                                                          
021100 01  KURSTABELL-FRF.                                                      
021200     03  FRF           OCCURS 12000  INDEXED BY FRF-IX.                   
021300         05 FRF-DATUM-FOM      PIC 9(8).                                  
021400         05 FILLER             PIC X.                                     
021500         05 FRF-DATUM-TOM      PIC 9(8).                                  
021600         05 FILLER             PIC X.                                     
021700         05 FRF-PRKURS         PIC 9(6)V9(5).                             
021800     EJECT                                                                
021900 01  KURSTABELL-GBP.                                                      
022000     03  GBP           OCCURS 12000  INDEXED BY GBP-IX.                   
022100         05 GBP-DATUM-FOM      PIC 9(8).                                  
022200         05 FILLER             PIC X.                                     
022300         05 GBP-DATUM-TOM      PIC 9(8).                                  
022400         05 FILLER             PIC X.                                     
022500         05 GBP-PRKURS         PIC 9(6)V9(5).                             
022600                                                                          
022700 01  KURSTABELL-HKD.                                                      
022800     03  HKD           OCCURS 12000  INDEXED BY HKD-IX.                   
022900         05 HKD-DATUM-FOM      PIC 9(8).                                  
023000         05 FILLER             PIC X.                                     
023100         05 HKD-DATUM-TOM      PIC 9(8).                                  
023200         05 FILLER             PIC X.                                     
023300         05 HKD-PRKURS         PIC 9(6)V9(5).                             
023400                                                                          
023500 01  KURSTABELL-HUF.                                                      
023600     03  HUF           OCCURS 12000  INDEXED BY HUF-IX.                   
023700         05 HUF-DATUM-FOM      PIC 9(8).                                  
023800         05 FILLER             PIC X.                                     
023900         05 HUF-DATUM-TOM      PIC 9(8).                                  
024000         05 FILLER             PIC X.                                     
024100         05 HUF-PRKURS         PIC 9(6)V9(5).                             
024200                                                                          
024300 01  KURSTABELL-IEP.                                                      
024400     03  IEP           OCCURS 12000  INDEXED BY IEP-IX.                   
024500         05 IEP-DATUM-FOM      PIC 9(8).                                  
024600         05 FILLER             PIC X.                                     
024700         05 IEP-DATUM-TOM      PIC 9(8).                                  
024800         05 FILLER             PIC X.                                     
024900         05 IEP-PRKURS         PIC 9(6)V9(5).                             
025000                                                                          
025100 01  KURSTABELL-INR.                                                      
025200     03  INR           OCCURS 12000  INDEXED BY INR-IX.                   
025300         05 INR-DATUM-FOM      PIC 9(8).                                  
025400         05 FILLER             PIC X.                                     
025500         05 INR-DATUM-TOM      PIC 9(8).                                  
025600         05 FILLER             PIC X.                                     
025700         05 INR-PRKURS         PIC 9(6)V9(5).                             
025800     EJECT                                                                
025900 01  KURSTABELL-ITL.                                                      
026000     03  ITL           OCCURS 12000  INDEXED BY ITL-IX.                   
026100         05 ITL-DATUM-FOM      PIC 9(8).                                  
026200         05 FILLER             PIC X.                                     
026300         05 ITL-DATUM-TOM      PIC 9(8).                                  
026400         05 FILLER             PIC X.                                     
026500         05 ITL-PRKURS         PIC 9(6)V9(5).                             
026600                                                                          
026700 01  KURSTABELL-JPY.                                                      
026800     03  JPY           OCCURS 12000  INDEXED BY JPY-IX.                   
026900         05 JPY-DATUM-FOM      PIC 9(8).                                  
027000         05 FILLER             PIC X.                                     
027100         05 JPY-DATUM-TOM      PIC 9(8).                                  
027200         05 FILLER             PIC X.                                     
027300         05 JPY-PRKURS         PIC 9(6)V9(5).                             
027400                                                                          
027500 01  KURSTABELL-KRW.                                                      
027600     03  KRW           OCCURS 12000  INDEXED BY KRW-IX.                   
027700         05 KRW-DATUM-FOM      PIC 9(8).                                  
027800         05 FILLER             PIC X.                                     
027900         05 KRW-DATUM-TOM      PIC 9(8).                                  
028000         05 FILLER             PIC X.                                     
028100         05 KRW-PRKURS         PIC 9(6)V9(5).                             
028200                                                                          
028300 01  KURSTABELL-LBP.                                                      
028400     03  LBP           OCCURS 12000  INDEXED BY LBP-IX.                   
028500         05 LBP-DATUM-FOM      PIC 9(8).                                  
028600         05 FILLER             PIC X.                                     
028700         05 LBP-DATUM-TOM      PIC 9(8).                                  
028800         05 FILLER             PIC X.                                     
028900         05 LBP-PRKURS         PIC 9(6)V9(5).                             
029000                                                                          
029100 01  KURSTABELL-MXN.                                                      
029200     03  MXN           OCCURS 12000  INDEXED BY MXN-IX.                   
029300         05 MXN-DATUM-FOM      PIC 9(8).                                  
029400         05 FILLER             PIC X.                                     
029500         05 MXN-DATUM-TOM      PIC 9(8).                                  
029600         05 FILLER             PIC X.                                     
029700         05 MXN-PRKURS         PIC 9(6)V9(5).                             
029800                                                                          
029100 01  KURSTABELL-ZAR.                                                      
029200     03  ZAR           OCCURS 12000  INDEXED BY ZAR-IX.                   
029300         05 ZAR-DATUM-FOM      PIC 9(8).                                  
029400         05 FILLER             PIC X.                                     
029500         05 ZAR-DATUM-TOM      PIC 9(8).                                  
029600         05 FILLER             PIC X.                                     
029700         05 ZAR-PRKURS         PIC 9(6)V9(5).                             
029800                                                                          
029900 01  KURSTABELL-MYR.                                                      
030000     03  MYR           OCCURS 12000  INDEXED BY MYR-IX.                   
030100         05 MYR-DATUM-FOM      PIC 9(8).                                  
030200         05 FILLER             PIC X.                                     
030300         05 MYR-DATUM-TOM      PIC 9(8).                                  
030400         05 FILLER             PIC X.                                     
030500         05 MYR-PRKURS         PIC 9(6)V9(5).                             
030600     EJECT                                                                
030700 01  KURSTABELL-NLG.                                                      
030800     03  NLG           OCCURS 12000  INDEXED BY NLG-IX.                   
030900         05 NLG-DATUM-FOM      PIC 9(8).                                  
031000         05 FILLER             PIC X.                                     
031100         05 NLG-DATUM-TOM      PIC 9(8).                                  
031200         05 FILLER             PIC X.                                     
031300         05 NLG-PRKURS         PIC 9(6)V9(5).                             
031400                                                                          
031500 01  KURSTABELL-NOK.                                                      
031600     03  NOK           OCCURS 12000  INDEXED BY NOK-IX.                   
031700         05 NOK-DATUM-FOM      PIC 9(8).                                  
031800         05 FILLER             PIC X.                                     
031900         05 NOK-DATUM-TOM      PIC 9(8).                                  
032000         05 FILLER             PIC X.                                     
032100         05 NOK-PRKURS         PIC 9(6)V9(5).                             
032200                                                                          
032300 01  KURSTABELL-PEI.                                                      
032400     03  PEI           OCCURS 12000  INDEXED BY PEI-IX.                   
032500         05 PEI-DATUM-FOM      PIC 9(8).                                  
032600         05 FILLER             PIC X.                                     
032700         05 PEI-DATUM-TOM      PIC 9(8).                                  
032800         05 FILLER             PIC X.                                     
032900         05 PEI-PRKURS         PIC 9(6)V9(5).                             
033000                                                                          
033100 01  KURSTABELL-PLN.                                                      
033200     03  PLN           OCCURS 12000  INDEXED BY PLN-IX.                   
033300         05 PLN-DATUM-FOM      PIC 9(8).                                  
033400         05 FILLER             PIC X.                                     
033500         05 PLN-DATUM-TOM      PIC 9(8).                                  
033600         05 FILLER             PIC X.                                     
033700         05 PLN-PRKURS         PIC 9(6)V9(5).                             
033800                                                                          
033900 01  KURSTABELL-PTE.                                                      
034000     03  PTE           OCCURS 12000  INDEXED BY PTE-IX.                   
034100         05 PTE-DATUM-FOM      PIC 9(8).                                  
034200         05 FILLER             PIC X.                                     
034300         05 PTE-DATUM-TOM      PIC 9(8).                                  
034400         05 FILLER             PIC X.                                     
034500         05 PTE-PRKURS         PIC 9(6)V9(5).                             
034600     EJECT                                                                
034700 01  KURSTABELL-SAR.                                                      
034800     03  SAR           OCCURS 12000  INDEXED BY SAR-IX.                   
034900         05 SAR-DATUM-FOM      PIC 9(8).                                  
035000         05 FILLER             PIC X.                                     
035100         05 SAR-DATUM-TOM      PIC 9(8).                                  
035200         05 FILLER             PIC X.                                     
035300         05 SAR-PRKURS         PIC 9(6)V9(5).                             
035400                                                                          
035500 01  KURSTABELL-SEK.                                                      
035600     03  SEK           OCCURS    1   INDEXED BY SEK-IX.                   
035700         05 SEK-DATUM-FOM      PIC 9(8).                                  
035800         05 FILLER             PIC X.                                     
035900         05 SEK-DATUM-TOM      PIC 9(8).                                  
036000         05 FILLER             PIC X.                                     
036100         05 SEK-PRKURS         PIC 9(6)V9(5).                             
036200                                                                          
036300 01  KURSTABELL-SGD.                                                      
036400     03  SGD           OCCURS 12000  INDEXED BY SGD-IX.                   
036500         05 SGD-DATUM-FOM      PIC 9(8).                                  
036600         05 FILLER             PIC X.                                     
036700         05 SGD-DATUM-TOM      PIC 9(8).                                  
036800         05 FILLER             PIC X.                                     
036900         05 SGD-PRKURS         PIC 9(6)V9(5).                             
037000                                                                          
037100 01  KURSTABELL-SKK.                                                      
037200     03  SKK           OCCURS 12000  INDEXED BY SKK-IX.                   
037300         05 SKK-DATUM-FOM      PIC 9(8).                                  
037400         05 FILLER             PIC X.                                     
037500         05 SKK-DATUM-TOM      PIC 9(8).                                  
037600         05 FILLER             PIC X.                                     
037700         05 SKK-PRKURS         PIC 9(6)V9(5).                             
037800                                                                          
037900 01  KURSTABELL-THB.                                                      
038000     03  THB           OCCURS 12000  INDEXED BY THB-IX.                   
038100         05 THB-DATUM-FOM      PIC 9(8).                                  
038200         05 FILLER             PIC X.                                     
038300         05 THB-DATUM-TOM      PIC 9(8).                                  
038400         05 FILLER             PIC X.                                     
038500         05 THB-PRKURS         PIC 9(6)V9(5).                             
038600                                                                          
038700 01  KURSTABELL-TRL.                                                      
038800     03  TRL           OCCURS 12000  INDEXED BY TRL-IX.                   
038900         05 TRL-DATUM-FOM      PIC 9(8).                                  
039000         05 FILLER             PIC X.                                     
039100         05 TRL-DATUM-TOM      PIC 9(8).                                  
039200         05 FILLER             PIC X.                                     
039300         05 TRL-PRKURS         PIC 9(6)V9(5).                             
039400     EJECT                                                                
039500 01  KURSTABELL-TWD.                                                      
039600     03  TWD           OCCURS 12000  INDEXED BY TWD-IX.                   
039700         05 TWD-DATUM-FOM      PIC 9(8).                                  
039800         05 FILLER             PIC X.                                     
039900         05 TWD-DATUM-TOM      PIC 9(8).                                  
040000         05 FILLER             PIC X.                                     
040100         05 TWD-PRKURS         PIC 9(6)V9(5).                             
040200                                                                          
040300 01  KURSTABELL-USD.                                                      
040400     03  USD           OCCURS 12000  INDEXED BY USD-IX.                   
040500         05 USD-DATUM-FOM      PIC 9(8).                                  
040600         05 FILLER             PIC X.                                     
040700         05 USD-DATUM-TOM      PIC 9(8).                                  
040800         05 FILLER             PIC X.                                     
040900         05 USD-PRKURS         PIC 9(6)V9(5).                             
041000                                                                          
041100 01  KURSTABELL-EUR.                                                      
041200     03  EUR           OCCURS 12000  INDEXED BY EUR-IX.                   
041300         05 EUR-DATUM-FOM      PIC 9(8).                                  
041400         05 FILLER             PIC X.                                     
041500         05 EUR-DATUM-TOM      PIC 9(8).                                  
041600         05 FILLER             PIC X.                                     
041700         05 EUR-PRKURS         PIC 9(6)V9(5).                             
041800                                                                          
041900 01  KURSTABELL-YUN.                                                      
042000     03  YUN           OCCURS 12000  INDEXED BY YUN-IX.                   
042100         05 YUN-DATUM-FOM      PIC 9(8).                                  
042200         05 FILLER             PIC X.                                     
042300         05 YUN-DATUM-TOM      PIC 9(8).                                  
042400         05 FILLER             PIC X.                                     
042500         05 YUN-PRKURS         PIC 9(6)V9(5).                             
042600                                                                          
042700     EJECT                                                                
042710 01  KURSTABELL-AED.                                                      
042720     03  AED           OCCURS 12000  INDEXED BY AED-IX.                   
042730         05 AED-DATUM-FOM      PIC 9(8).                                  
042740         05 FILLER             PIC X.                                     
042750         05 AED-DATUM-TOM      PIC 9(8).                                  
042760         05 FILLER             PIC X.                                     
042770         05 AED-PRKURS         PIC 9(6)V9(5).                             
042780                                                                          
042790     EJECT                                                                
042791 01  KURSTABELL-TRY.                                                      
042792     03  TRY           OCCURS 12000  INDEXED BY TRY-IX.                   
042793         05 TRY-DATUM-FOM      PIC 9(8).                                  
042794         05 FILLER             PIC X.                                     
042795         05 TRY-DATUM-TOM      PIC 9(8).                                  
042796         05 FILLER             PIC X.                                     
042797         05 TRY-PRKURS         PIC 9(6)V9(5).                             
042798                                                                          
042799     EJECT                                                                
042800 01  DYNAMISKA-SUBPROGRAM.                                                
042900*                                                                         
043000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
043100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
043200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
043300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
043400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
043500     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
043600     SKIP2                                                                
043700*    --- PARAMETRAR TILL ABEND                                            
043800                                                                          
043900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
044000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
044100     SKIP2                                                                
044200 01  FELTEXT.                                                             
044300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
044400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
044500     EJECT                                                                
044600*    --- PARAMETRAR TILL DATKORT                                          
044700*                                                                         
044800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51238'.              
044900     SKIP2                                                                
045000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
045100     SKIP2                                                                
045200*01  -COPY WDATKORT                                                       
045300     EJECT                                                                
045400*    --- PARAMETRAR TILL POSTSUM                                          
045500*                                                                         
045600*01  -COPY W0005   -PRE  POSTSUM-                                         
045700     EJECT                                                                
045800 01  K-AREA-START                PIC X(24)   VALUE                        
045900                                 'K-AREA-START  '.                        
046000*01  AREA -COPY W51237     -PRE K-                                        
046100     EJECT                                                                
046200 01  IN-AREA-START               PIC X(24)   VALUE                        
046300                                 'IN-AREA-START  '.                       
046400 01  IN-AREA.                                                             
046500*    03   -COPY W51233 -PRE IN-                                           
046600     EJECT                                                                
046700 01  UT1-AREA-START              PIC X(24)   VALUE                        
046800                                 'UT1-AREA-START  '.                      
046900     SKIP2                                                                
047000                                                                          
047100*01  AREA -COPY W51239     -PRE UT1-                                      
047200     EJECT                                                                
047300 01  UT2-AREA-START              PIC X(24)   VALUE                        
047400                                 'UT2-AREA-START  '.                      
047500     SKIP2                                                                
047600                                                                          
047700*01  AREA -COPY W51239     -PRE UT2-                                      
047800     EJECT                                                                
047900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
048000*                                                                         
048100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
048200     SKIP3                                                                
048300 01  NYCKLAR-TILL-DLI.                                                    
048400     03  W-IDLEVNR-X.                                                     
048500         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
048510     03  W-IDLAND-X.                                                      
048520         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
048600     03  W-IDARTNR-X.                                                     
048700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
048800     03  W-DAINLEV-X.                                                     
048900         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
049000     SKIP2                                                                
049100*    --- STATUS-KOD FRÅN IMS                                              
049200 01  STATUS-WS                   PIC XX.                                  
049300     88  SEGMENT-FINNS                       VALUE '  '.                  
049400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
049500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
049600     SKIP2                                                                
049700 01  GODK-STATUSKODER.                                                    
049800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
049900     SKIP3                                                                
050000 01  SSA1                        PIC X(64).                               
050100 01  SSA2                        PIC X(64).                               
050200     EJECT                                                                
050300*    --- IMS FUNKTIONSKODER                                               
050400*01  -COPY W0003                                                          
050500     EJECT                                                                
050600*    ---  DLI INPUT-OUTPUT AREA                                           
050700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
050800     SKIP3                                                                
050900 01  DLI-IO-AREA.                                                         
051000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
051100     SKIP3                                                                
051200     03  WLLEVA01 REDEFINES IO-AREA.                                      
051300*        05  -COPY WDF101                                                 
051400     EJECT                                                                
051500     03  WLLEVA11 REDEFINES IO-AREA.                                      
051600*        05  -COPY WDF102                                                 
051700     EJECT                                                                
051800     03  WLINLE01 REDEFINES IO-AREA.                                      
051900*        05  -COPY WDL201                                                 
052000     EJECT                                                                
052100     03  WLINLE11 REDEFINES IO-AREA.                                      
052200*        05  -COPY WDL211                                                 
052300     EJECT                                                                
052400     03  WLINLE21 REDEFINES IO-AREA.                                      
052500*        05  -COPY WDL221                                                 
052600     EJECT                                                                
052700     03  WLINLE22 REDEFINES IO-AREA.                                      
052800*        05  -COPY WDL222                                                 
052900     EJECT                                                                
053000 LINKAGE SECTION.                                                         
053100                                                                          
053200                                                                          
053300*01  -COPY W0008  -PRE LEVA-                                              
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600*01  -COPY W0008  -PRE INLE-                                              
053700     05  FILLER                  PIC X.                                   
053800     EJECT                                                                
053900 PROCEDURE DIVISION  USING LEVA-PCB INLE-PCB.                             
054000 MAIN SECTION.                                                            
054100     ENTRY 'DLITCBL' USING LEVA-PCB INLE-PCB.                             
054200                                                                          
054300                                                                          
054400     PERFORM A-INIT                                                       
054500                                                                          
054600     PERFORM AA-LAS-IN-KURSER-I-TABELLER                                  
054700                                                                          
054800     SORT SORTFIL ASCENDING SORT-RANDOMKEY                                
054900                            SORT-IDARTNR                                  
055000         INPUT PROCEDURE C-BESTAM-RANDOMKEY                               
055100         OUTPUT PROCEDURE D-BEARBETA-SORTFIL                              
055200                                                                          
055300     PERFORM Z-FINIT                                                      
055400     MOVE ZERO TO RETURN-CODE                                             
055500     GOBACK                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 A-INIT SECTION.                                                          
055900                                                                          
056000     OPEN INPUT  KURSFIL                                                  
056100                 INFIL                                                    
056200                                                                          
056300     OPEN OUTPUT UTFIL1                                                   
056400                 UTFIL2                                                   
056500                                                                          
056600                                                                          
056700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
056800     MOVE 20            TO AKT-DADATUM(1:2)                               
056900     MOVE D-AAR         TO AKT-DADATUM(3:2)                               
057000     MOVE D-MAANAD      TO AKT-DADATUM(5:2)                               
057100     MOVE D-DAG         TO AKT-DADATUM(7:2)                               
057200     MOVE AKT-DADATUM(3:6) TO AKT-TIDATUM                                 
057300                                                                          
057400     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
057500     .                                                                    
057600     EJECT                                                                
057700 AA-LAS-IN-KURSER-I-TABELLER SECTION.                                     
057800     SKIP2                                                                
057900     PERFORM AAA-FLYTTA-HIGH-VALUES                                       
058000     PERFORM AAB-FLYTTA-ETT-TILL-INDEX                                    
058100                                                                          
058200     PERFORM S01-LAES-KURSFIL                                             
058300                                                                          
058400     PERFORM UNTIL KURSFIL-EOF-SW = JA                                    
058500       EVALUATE K-KDVALISO                                                
058600         WHEN 'ATS' MOVE K-AREA(5:29) TO ATS (ATS-IX)                     
058700                    SET ATS-IX UP BY +1                                   
058800         WHEN 'AUD' MOVE K-AREA(5:29) TO AUD (AUD-IX)                     
058900                    SET AUD-IX UP BY +1                                   
059000         WHEN 'BEF' MOVE K-AREA(5:29) TO BEF (BEF-IX)                     
059100                    SET BEF-IX UP BY +1                                   
059110         WHEN 'BRL' MOVE K-AREA(5:29) TO BRL (BRL-IX)                     
059120                    SET BRL-IX UP BY +1                                   
059200         WHEN 'CAD' MOVE K-AREA(5:29) TO CAD (CAD-IX)                     
059300                    SET CAD-IX UP BY +1                                   
059400         WHEN 'CHF' MOVE K-AREA(5:29) TO CHF (CHF-IX)                     
059500                    SET CHF-IX UP BY +1                                   
059600         WHEN 'CLP' MOVE K-AREA(5:29) TO CLP (CLP-IX)                     
059700                    SET CLP-IX UP BY +1                                   
059710         WHEN 'CNY' MOVE K-AREA(5:29) TO CNY (CNY-IX)                     
059720                    SET CNY-IX UP BY +1                                   
059800         WHEN 'CSK' MOVE K-AREA(5:29) TO CSK (CSK-IX)                     
059900                    SET CSK-IX UP BY +1                                   
060000         WHEN 'CZK' MOVE K-AREA(5:29) TO CZK (CZK-IX)                     
060100                    SET CZK-IX UP BY +1                                   
060200         WHEN 'DEM' MOVE K-AREA(5:29) TO DEM (DEM-IX)                     
060300                    SET DEM-IX UP BY +1                                   
060400         WHEN 'DKK' MOVE K-AREA(5:29) TO DKK (DKK-IX)                     
060500                    SET DKK-IX UP BY +1                                   
060600         WHEN 'ESP' MOVE K-AREA(5:29) TO ESP (ESP-IX)                     
060700                    SET ESP-IX UP BY +1                                   
060800         WHEN 'FIM' MOVE K-AREA(5:29) TO FIM (FIM-IX)                     
060900                    SET FIM-IX UP BY +1                                   
061000         WHEN 'FRF' MOVE K-AREA(5:29) TO FRF (FRF-IX)                     
061100                    SET FRF-IX UP BY +1                                   
061200         WHEN 'GBP' MOVE K-AREA(5:29) TO GBP (GBP-IX)                     
061300                    SET GBP-IX UP BY +1                                   
061400         WHEN 'HKD' MOVE K-AREA(5:29) TO HKD (HKD-IX)                     
061500                    SET HKD-IX UP BY +1                                   
061600         WHEN 'HUF' MOVE K-AREA(5:29) TO HUF (HUF-IX)                     
061700                    SET HUF-IX UP BY +1                                   
061800         WHEN 'IEP' MOVE K-AREA(5:29) TO IEP (IEP-IX)                     
061900                    SET IEP-IX UP BY +1                                   
062000         WHEN 'INR' MOVE K-AREA(5:29) TO INR (INR-IX)                     
062100                    SET INR-IX UP BY +1                                   
062200         WHEN 'ITL' MOVE K-AREA(5:29) TO ITL (ITL-IX)                     
062300                    SET ITL-IX UP BY +1                                   
062400         WHEN 'JPY' MOVE K-AREA(5:29) TO JPY (JPY-IX)                     
062500                    SET JPY-IX UP BY +1                                   
062600         WHEN 'KRW' MOVE K-AREA(5:29) TO KRW (KRW-IX)                     
062700                    SET KRW-IX UP BY +1                                   
062800         WHEN 'LBP' MOVE K-AREA(5:29) TO LBP (LBP-IX)                     
062900                    SET LBP-IX UP BY +1                                   
063000         WHEN 'MXN' MOVE K-AREA(5:29) TO MXN (MXN-IX)                     
063100                    SET MXN-IX UP BY +1                                   
063000         WHEN 'ZAR' MOVE K-AREA(5:29) TO ZAR (ZAR-IX)                     
063100                    SET ZAR-IX UP BY +1                                   
063200         WHEN 'MYR' MOVE K-AREA(5:29) TO MYR (MYR-IX)                     
063300                    SET MYR-IX UP BY +1                                   
063400         WHEN 'NLG' MOVE K-AREA(5:29) TO NLG (NLG-IX)                     
063500                    SET NLG-IX UP BY +1                                   
063600         WHEN 'NOK' MOVE K-AREA(5:29) TO NOK (NOK-IX)                     
063700                    SET NOK-IX UP BY +1                                   
063800         WHEN 'PEI' MOVE K-AREA(5:29) TO PEI (PEI-IX)                     
063900                    SET PEI-IX UP BY +1                                   
064000         WHEN 'PLN' MOVE K-AREA(5:29) TO PLN (PLN-IX)                     
064100                    SET PLN-IX UP BY +1                                   
064200         WHEN 'PTE' MOVE K-AREA(5:29) TO PTE (PTE-IX)                     
064300                    SET PTE-IX UP BY +1                                   
064400         WHEN 'SAR' MOVE K-AREA(5:29) TO SAR (SAR-IX)                     
064500                    SET SAR-IX UP BY +1                                   
064600         WHEN 'SEK' MOVE K-AREA(5:29) TO SEK (SEK-IX)                     
064700                    SET SEK-IX UP BY +1                                   
064800         WHEN 'SGD' MOVE K-AREA(5:29) TO SGD (SGD-IX)                     
064900                    SET SGD-IX UP BY +1                                   
065000         WHEN 'SKK' MOVE K-AREA(5:29) TO SKK (SKK-IX)                     
065100                    SET SKK-IX UP BY +1                                   
065200         WHEN 'THB' MOVE K-AREA(5:29) TO THB (THB-IX)                     
065300                    SET THB-IX UP BY +1                                   
065400         WHEN 'TRL' MOVE K-AREA(5:29) TO TRL (TRL-IX)                     
065500                    SET TRL-IX UP BY +1                                   
065600         WHEN 'TWD' MOVE K-AREA(5:29) TO TWD (TWD-IX)                     
065700                    SET TWD-IX UP BY +1                                   
065800         WHEN 'USD' MOVE K-AREA(5:29) TO USD (USD-IX)                     
065900                    SET USD-IX UP BY +1                                   
066000         WHEN 'EUR' MOVE K-AREA(5:29) TO EUR (EUR-IX)                     
066100                    SET EUR-IX UP BY +1                                   
066200         WHEN 'YUN' MOVE K-AREA(5:29) TO YUN (YUN-IX)                     
066300                    SET YUN-IX UP BY +1                                   
066310         WHEN 'AED' MOVE K-AREA(5:29) TO AED (AED-IX)                     
066320                    SET AED-IX UP BY +1                                   
066330         WHEN 'TRY' MOVE K-AREA(5:29) TO TRY (TRY-IX)                     
066340                    SET TRY-IX UP BY +1                                   
066400         WHEN OTHER DISPLAY '  '                                          
066500                    DISPLAY 'KURS FÖR VALUTA ' K-KDVALISO                 
066600                            ' HAR EJ LÄSTS IN I KURSTABELL '              
066700       END-EVALUATE                                                       
066800                                                                          
066900       PERFORM S01-LAES-KURSFIL                                           
067000     END-PERFORM                                                          
067100     .                                                                    
067200     EJECT                                                                
067300 AAA-FLYTTA-HIGH-VALUES SECTION.                                          
067400     SKIP2                                                                
067500     MOVE HIGH-VALUES TO KURSTABELL-ATS                                   
067600                         KURSTABELL-AUD                                   
067700                         KURSTABELL-BEF                                   
067710                         KURSTABELL-BRL                                   
067800                         KURSTABELL-CAD                                   
067900                         KURSTABELL-CHF                                   
068000                         KURSTABELL-CLP                                   
068010                         KURSTABELL-CNY                                   
068100                         KURSTABELL-CSK                                   
068200                         KURSTABELL-CZK                                   
068300                         KURSTABELL-DEM                                   
068400                         KURSTABELL-DKK                                   
068500                         KURSTABELL-ESP                                   
068600                         KURSTABELL-FIM                                   
068700                         KURSTABELL-FRF                                   
068800                         KURSTABELL-GBP                                   
068900                         KURSTABELL-HKD                                   
069000                         KURSTABELL-HUF                                   
069100                         KURSTABELL-IEP                                   
069200                         KURSTABELL-INR                                   
069300                         KURSTABELL-ITL                                   
069400                         KURSTABELL-JPY                                   
069500                         KURSTABELL-KRW                                   
069600                         KURSTABELL-LBP                                   
069700                         KURSTABELL-MXN                                   
069700                         KURSTABELL-ZAR                                   
069800                         KURSTABELL-MYR                                   
069900                         KURSTABELL-NLG                                   
070000                         KURSTABELL-NOK                                   
070100                         KURSTABELL-PEI                                   
070200                         KURSTABELL-PLN                                   
070300                         KURSTABELL-PTE                                   
070400                         KURSTABELL-SAR                                   
070500                         KURSTABELL-SEK                                   
070600                         KURSTABELL-SGD                                   
070700                         KURSTABELL-SKK                                   
070800                         KURSTABELL-THB                                   
070900                         KURSTABELL-TRL                                   
071000                         KURSTABELL-TWD                                   
071100                         KURSTABELL-USD                                   
071200                         KURSTABELL-EUR                                   
071300                         KURSTABELL-YUN                                   
071310                         KURSTABELL-AED                                   
071320                         KURSTABELL-TRY                                   
071400     .                                                                    
071500     EJECT                                                                
071600 AAB-FLYTTA-ETT-TILL-INDEX SECTION.                                       
071700     SKIP2                                                                
071800     SET ATS-IX TO +1                                                     
071900     SET AUD-IX TO +1                                                     
072000     SET BEF-IX TO +1                                                     
072010     SET BRL-IX TO +1                                                     
072100     SET CAD-IX TO +1                                                     
072200     SET CHF-IX TO +1                                                     
072300     SET CLP-IX TO +1                                                     
072310     SET CNY-IX TO +1                                                     
072400     SET CSK-IX TO +1                                                     
072500     SET CZK-IX TO +1                                                     
072600     SET DEM-IX TO +1                                                     
072700     SET DKK-IX TO +1                                                     
072800     SET ESP-IX TO +1                                                     
072900     SET FIM-IX TO +1                                                     
073000     SET FRF-IX TO +1                                                     
073100     SET GBP-IX TO +1                                                     
073200     SET HKD-IX TO +1                                                     
073300     SET HUF-IX TO +1                                                     
073400     SET IEP-IX TO +1                                                     
073500     SET INR-IX TO +1                                                     
073600     SET ITL-IX TO +1                                                     
073700     SET JPY-IX TO +1                                                     
073800     SET KRW-IX TO +1                                                     
073900     SET LBP-IX TO +1                                                     
074000     SET MXN-IX TO +1                                                     
074000     SET ZAR-IX TO +1                                                     
074100     SET MYR-IX TO +1                                                     
074200     SET NLG-IX TO +1                                                     
074300     SET NOK-IX TO +1                                                     
074400     SET PEI-IX TO +1                                                     
074500     SET PLN-IX TO +1                                                     
074600     SET PTE-IX TO +1                                                     
074700     SET SAR-IX TO +1                                                     
074800     SET SEK-IX TO +1                                                     
074900     SET SGD-IX TO +1                                                     
075000     SET SKK-IX TO +1                                                     
075100     SET THB-IX TO +1                                                     
075200     SET TRL-IX TO +1                                                     
075300     SET TWD-IX TO +1                                                     
075400     SET USD-IX TO +1                                                     
075500     SET EUR-IX TO +1                                                     
075600     SET YUN-IX TO +1                                                     
075610     SET AED-IX TO +1                                                     
075620     SET TRY-IX TO +1                                                     
075700     .                                                                    
075800     EJECT                                                                
075900 C-BESTAM-RANDOMKEY SECTION.                                              
076000                                                                          
076100     PERFORM S02-LAES-INFIL                                               
076200     PERFORM UNTIL INFIL-EOF-SW = JA                                      
076300       CALL W015RAND USING SORT-IDARTNR SORT-RANDOMKEY DATABASE           
076400                                                                          
076500       RELEASE SORT-RECORD                                                
076600       PERFORM S02-LAES-INFIL                                             
076700     END-PERFORM                                                          
076800     .                                                                    
076900     EJECT                                                                
077000 D-BEARBETA-SORTFIL SECTION.                                              
077100                                                                          
077200     PERFORM S03-LAES-SORT                                                
077300                                                                          
077400     PERFORM UNTIL SORTFIL-EOF-SW = JA                                    
077500                                                                          
077600       COMPUTE SALDO = IN-KVLS + IN-KVAKS + IN-KVEFRS                     
077700                                                                          
077800       IF IN-RETULF  = +0                                                 
077900         PERFORM DA-HEMTAGNINGSFAKT-FRAN-LEVREG                           
078000       END-IF                                                             
078100                                                                          
078200       PERFORM DD-FLYTTA-TILL-UTFILER                                     
078300                                                                          
078400       MOVE IN-IDARTNR TO W-IDARTNR                                       
078500       PERFORM IMS-GU-WLINLE01                                            
078600                                                                          
078700       IF SEGMENT-FINNS                                                   
078800         PERFORM IMS-GNP-WLINLE11                                         
078900                                                                          
079000         PERFORM UNTIL SEGMENT-SAKNAS OR SALDO = +0                       
079100           MOVE INL-DAINLEV TO W-DAINLEV                                  
079200           PERFORM IMS-GNP-WLINLE21                                       
079300           IF SEGMENT-FINNS                                               
079400                                                                          
079500             MOVE MOT-TIAVIDAT    TO W-DAAVIDAT(2:7)                      
079600             IF W-DAAVIDAT(3:2) < 50                                      
079700               MOVE 20            TO W-DAAVIDAT(1:2)                      
079800             ELSE                                                         
079900               MOVE 19            TO W-DAAVIDAT(1:2)                      
080000             END-IF                                                       
080100             IF (MOT-KDRT = +0 OR +3 OR +6 OR +9)     AND                 
080200                            (W-DAAVIDAT <= AKT-DADATUM)                   
080300                                                                          
080400               MOVE W-DAAVIDAT      TO W-INLEV-DATUM                      
080500                                                                          
080600               IF MOT-IDPTYP = 'R31'                                      
080700                 PERFORM DE-R31-I-RATT-GRUPP                              
080800               ELSE                                                       
080900                 PERFORM DF-R32-I-RATT-GRUPP                              
081000               END-IF                                                     
081100             END-IF                                                       
081200           ELSE                                                           
081300             PERFORM IMS-GNP-WLINLE22                                     
081400             IF SEGMENT-FINNS                                             
081500                                                                          
081600               MOVE DIR-TIAVSDAT    TO W-DAAVSDAT(2:7)                    
081700               IF W-DAAVSDAT(3:2) < 50                                    
081800                 MOVE 20            TO W-DAAVSDAT(1:2)                    
081900               ELSE                                                       
082000                 MOVE 19            TO W-DAAVSDAT(1:2)                    
082100               END-IF                                                     
082200               IF DIR-IDPTYP = 'R34'                                      
082300                 IF (DIR-KDRT = +0 OR +3 OR +6 OR +9) AND                 
082400                    (W-DAAVSDAT NOT > AKT-DADATUM)                        
082500                                                                          
082600                   MOVE W-DAAVSDAT TO W-INLEV-DATUM                       
082700                                                                          
082800                   PERFORM DG-R34-I-RATT-GRUPP                            
082900                 END-IF                                                   
083000               END-IF                                                     
083100             END-IF                                                       
083200           END-IF                                                         
083300           PERFORM IMS-GNP-WLINLE11                                       
083400         END-PERFORM                                                      
083500                                                                          
083600         PERFORM S12-SKRIV-UTFIL2                                         
083700                                                                          
083800         IF SALDO > 0                                                     
083900           PERFORM DH-LAGG-RESTEN-I-RATT-GRUPP                            
084000         END-IF                                                           
084100         PERFORM S11-SKRIV-UTFIL1                                         
084200       ELSE                                                               
084300         PERFORM DH-LAGG-RESTEN-I-RATT-GRUPP                              
084400         PERFORM S11-SKRIV-UTFIL1                                         
084500       END-IF                                                             
084600                                                                          
084700       PERFORM S03-LAES-SORT                                              
084800     END-PERFORM                                                          
084900     .                                                                    
085000     EJECT                                                                
085100 DA-HEMTAGNINGSFAKT-FRAN-LEVREG SECTION.                                  
085200     SKIP3                                                                
085300     MOVE IN-IDLEVNR      TO W-IDLEVNR                                    
085400     PERFORM IMS-GU-WLLEVA01                                              
085500     IF SEGMENT-FINNS                                                     
085510       MOVE 'SE' TO W-IDLAND                                              
085600       PERFORM IMS-GNP-WLLEVA11                                           
085700       IF SEGMENT-FINNS                                                   
085800         MOVE TULL-TITULF    TO TMP1-YYMMDD                               
085900         MOVE AKT-TIDATUM    TO TMP2-YYMMDD                               
086000         PERFORM WY2000P1                                                 
086100         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
086200           MOVE TULL-RETULF-1  TO IN-RETULF                               
086300         ELSE                                                             
086400           MOVE TULL-RETULF-2  TO IN-RETULF                               
086500         END-IF                                                           
086600       ELSE                                                               
086700         MOVE +1.0812     TO IN-RETULF                                    
086800         DISPLAY '  '                                                     
086900         DISPLAY 'FÖR LEVERANTÖR ' W-IDLEVNR                              
087000         DISPLAY 'SAKNAS HEMTAGNINGSFAKTOR PÅ '                           
087100                 'LEVERANTÖRSREGISTRET '                                  
087200       END-IF                                                             
087300     ELSE                                                                 
087400       MOVE +1.0812       TO IN-RETULF                                    
087500       DISPLAY '  '                                                       
087600       DISPLAY 'LEVERANTÖRSNUMMER ' W-IDLEVNR ' SAKNAS PÅ '               
087700               'LEVERANTÖRSREGISTRET '                                    
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 DD-FLYTTA-TILL-UTFILER SECTION.                                          
088200     SKIP3                                                                
088300     MOVE IN-IDARTNR         TO UT1-IDARTNR                               
088400                                UT2-IDARTNR                               
088500     MOVE IN-IDLEVNR         TO UT1-IDLEVNR                               
088600                                UT2-IDLEVNR                               
088700     MOVE IN-KDPRODSL        TO UT1-KDPRODSL                              
088800                                UT2-KDPRODSL                              
088900     MOVE IN-KVAKS           TO UT1-KVAKS                                 
089000                                UT2-KVAKS                                 
089100     MOVE IN-KVEFRS          TO UT1-KVEFRS                                
089200                                UT2-KVEFRS                                
089300     MOVE IN-KVLS            TO UT1-KVLS                                  
089400                                UT2-KVLS                                  
089500     MOVE IN-PRINK           TO UT1-PRINK                                 
089600                                UT2-PRINK                                 
089700     MOVE IN-RETULF          TO UT1-RETULF                                
089800                                UT2-RETULF                                
089900                                                                          
090000     COMPUTE UT1-SUARTVLV-INK ROUNDED = SALDO * IN-PRINK /                
090100                                        IN-RETULF                         
090200     MOVE UT1-SUARTVLV-INK   TO UT2-SUARTVLV-INK                          
090300                                                                          
090400     EJECT                                                                
090500     MOVE +1 TO IX                                                        
090600                                                                          
090700     PERFORM UNTIL IX > +5                                                
090800                                                                          
090900       MOVE IN-TIPRLIST    (IX)  TO UT1-TIPRLIST    (IX)                  
091000                                    UT2-TIPRLIST    (IX)                  
091100       MOVE IN-SUINLEV-PR  (IX)  TO UT1-SUINLEV-PR  (IX)                  
091200                                    UT2-SUINLEV-PR  (IX)                  
091300       MOVE IN-PRARTBEL-PR (IX)  TO UT1-PRARTBEL-PR (IX)                  
091400                                    UT2-PRARTBEL-PR (IX)                  
091500       MOVE IN-KDVALISO    (IX)  TO UT1-KDVALISO    (IX)                  
091600                                    UT2-KDVALISO    (IX)                  
091700                                                                          
091800       MOVE ZERO                 TO UT1-KVANTMOT    (IX)                  
091900                                    UT2-KVANTMOT    (IX)                  
092000                                    UT1-SUARTVLV-PR (IX)                  
092100                                    UT2-SUARTVLV-PR (IX)                  
092200       ADD +1 TO IX                                                       
092300     END-PERFORM                                                          
092400     .                                                                    
092500     EJECT                                                                
092600 DE-R31-I-RATT-GRUPP SECTION.                                             
092700                                                                          
092800     MOVE +1 TO IX                                                        
092900                                                                          
093000     PERFORM UNTIL IX > +5                                                
093100       MOVE MOT-TIAVIDAT    TO TMP1-YYMMDD                                
093200       MOVE IN-TIPRLIST(IX) TO TMP2-YYMMDD                                
093300       PERFORM WY2000P1                                                   
093400       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
093500                                                                          
093600         IF MOT-KVAVIS             > SALDO                                
093700           ADD SALDO              TO UT1-KVANTMOT (IX)                    
093800           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
093900           PERFORM X-SOK-KURS                                             
094000           COMPUTE W-SUARTVLV ROUNDED = SALDO *                           
094100                              IN-PRARTBEL-PR (IX) * W-PRKURS              
094200           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
094300                                          W-SUARTVLV                      
094400           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
094500                                                                          
094600           MOVE ZERO              TO SALDO                                
094700         ELSE                                                             
094800           ADD MOT-KVAVIS         TO UT1-KVANTMOT (IX)                    
094900           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
095000           PERFORM X-SOK-KURS                                             
095100           COMPUTE W-SUARTVLV ROUNDED = MOT-KVAVIS *                      
095200                              IN-PRARTBEL-PR (IX) * W-PRKURS              
095300           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
095400                                          W-SUARTVLV                      
095500           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
095600                                                                          
095700           COMPUTE SALDO      = SALDO - MOT-KVAVIS                        
095800         END-IF                                                           
095900                                                                          
096000         MOVE +5 TO IX                                                    
096100                                                                          
096200       ELSE                                                               
096300                                                                          
096400         IF IN-TIPRLIST (IX) = +9999999                                   
096500           MOVE +5 TO IX                                                  
096600         END-IF                                                           
096700                                                                          
096800       END-IF                                                             
096900                                                                          
097000       ADD +1 TO IX                                                       
097100     END-PERFORM                                                          
097200     .                                                                    
097300     EJECT                                                                
097400 DF-R32-I-RATT-GRUPP SECTION.                                             
097500                                                                          
097600     MOVE +1 TO IX                                                        
097700                                                                          
097800     PERFORM UNTIL IX > +5                                                
097900       MOVE MOT-TIAVIDAT    TO TMP1-YYMMDD                                
098000       MOVE IN-TIPRLIST(IX) TO TMP2-YYMMDD                                
098100       PERFORM WY2000P1                                                   
098200       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
098300                                                                          
098400         IF MOT-KVANTMOT           > SALDO                                
098500           ADD SALDO              TO UT1-KVANTMOT (IX)                    
098600           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
098700           PERFORM X-SOK-KURS                                             
098800           COMPUTE W-SUARTVLV ROUNDED = SALDO *                           
098900                              IN-PRARTBEL-PR (IX) * W-PRKURS              
099000           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
099100                                          W-SUARTVLV                      
099200           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
099300                                                                          
099400           MOVE ZERO              TO SALDO                                
099500         ELSE                                                             
099600           ADD MOT-KVANTMOT       TO UT1-KVANTMOT (IX)                    
099700           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
099800           PERFORM X-SOK-KURS                                             
099900           COMPUTE W-SUARTVLV ROUNDED = MOT-KVANTMOT *                    
100000                              IN-PRARTBEL-PR (IX) * W-PRKURS              
100100           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
100200                                          W-SUARTVLV                      
100300           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
100400                                                                          
100500           COMPUTE SALDO      = SALDO - MOT-KVANTMOT                      
100600         END-IF                                                           
100700                                                                          
100800         MOVE +5 TO IX                                                    
100900                                                                          
101000       ELSE                                                               
101100                                                                          
101200         IF IN-TIPRLIST (IX) = +9999999                                   
101300           MOVE +5 TO IX                                                  
101400         END-IF                                                           
101500                                                                          
101600       END-IF                                                             
101700                                                                          
101800       ADD +1 TO IX                                                       
101900     END-PERFORM                                                          
102000     .                                                                    
102100     EJECT                                                                
102200 DG-R34-I-RATT-GRUPP SECTION.                                             
102300                                                                          
102400     MOVE +1 TO IX                                                        
102500                                                                          
102600     PERFORM UNTIL IX > +5                                                
102700       MOVE DIR-TIAVSDAT    TO TMP1-YYMMDD                                
102800       MOVE IN-TIPRLIST(IX) TO TMP2-YYMMDD                                
102900       PERFORM WY2000P1                                                   
103000       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
103100                                                                          
103200         IF DIR-KVAVIS             > SALDO                                
103300           ADD SALDO              TO UT1-KVANTMOT (IX)                    
103400           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
103500           PERFORM X-SOK-KURS                                             
103600           COMPUTE W-SUARTVLV ROUNDED = SALDO *                           
103700                              IN-PRARTBEL-PR (IX) * W-PRKURS              
103800           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
103900                                          W-SUARTVLV                      
104000           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
104100                                                                          
104200           MOVE ZERO              TO SALDO                                
104300         ELSE                                                             
104400           ADD DIR-KVAVIS         TO UT1-KVANTMOT (IX)                    
104500           MOVE UT1-KVANTMOT (IX) TO UT2-KVANTMOT (IX)                    
104600           PERFORM X-SOK-KURS                                             
104700           COMPUTE W-SUARTVLV ROUNDED = DIR-KVAVIS *                      
104800                              IN-PRARTBEL-PR (IX) * W-PRKURS              
104900           COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +          
105000                                          W-SUARTVLV                      
105100           MOVE    UT1-SUARTVLV-PR(IX) TO UT2-SUARTVLV-PR (IX)            
105200                                                                          
105300           COMPUTE SALDO      = SALDO - DIR-KVAVIS                        
105400         END-IF                                                           
105500                                                                          
105600         MOVE +5 TO IX                                                    
105700                                                                          
105800       ELSE                                                               
105900                                                                          
106000         IF IN-TIPRLIST (IX) = +9999999                                   
106100           MOVE +5 TO IX                                                  
106200         END-IF                                                           
106300                                                                          
106400       END-IF                                                             
106500                                                                          
106600       ADD +1 TO IX                                                       
106700     END-PERFORM                                                          
106800     .                                                                    
106900     EJECT                                                                
107000 DH-LAGG-RESTEN-I-RATT-GRUPP SECTION.                                     
107100     SKIP3                                                                
107200     IF IN-TIPRLIST (5)     < +9999999                                    
107300       MOVE +5 TO IX                                                      
107400       PERFORM HA-VARDE-FOR-RESTEN                                        
107500     ELSE                                                                 
107600       IF IN-TIPRLIST (4)     < +9999999                                  
107700         MOVE +4 TO IX                                                    
107800         PERFORM HA-VARDE-FOR-RESTEN                                      
107900       ELSE                                                               
108000         IF IN-TIPRLIST (3)     < +9999999                                
108100           MOVE +3 TO IX                                                  
108200           PERFORM HA-VARDE-FOR-RESTEN                                    
108300         ELSE                                                             
108400           IF IN-TIPRLIST (2)     < +9999999                              
108500             MOVE +2 TO IX                                                
108600             PERFORM HA-VARDE-FOR-RESTEN                                  
108700           ELSE                                                           
108800             IF IN-TIPRLIST (1)     < +9999999                            
108900               MOVE +1 TO IX                                              
109000               PERFORM HA-VARDE-FOR-RESTEN                                
109100             ELSE                                                         
109200               ADD SALDO                   TO UT1-KVANTMOT (1)            
109300               COMPUTE UT1-SUARTVLV-PR (1) ROUNDED = SALDO *              
109400                                           IN-PRINK / IN-RETULF           
109500             END-IF                                                       
109600           END-IF                                                         
109700         END-IF                                                           
109800       END-IF                                                             
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200 HA-VARDE-FOR-RESTEN SECTION.                                             
110300                                                                          
110400     MOVE IN-TIPRLIST (IX)  TO W-INLEV-DATUM(2:7)                         
110500     IF W-INLEV-DATUM(3:2) < 50                                           
110600       MOVE '20'            TO W-INLEV-DATUM(1:2)                         
110700     ELSE                                                                 
110800       MOVE '19'            TO W-INLEV-DATUM(1:2)                         
110900     END-IF                                                               
111000                                                                          
111100     ADD SALDO              TO UT1-KVANTMOT (IX)                          
111200     PERFORM X-SOK-KURS                                                   
111300     COMPUTE W-SUARTVLV ROUNDED = SALDO *                                 
111400                        IN-PRARTBEL-PR (IX) * W-PRKURS                    
111500     COMPUTE UT1-SUARTVLV-PR (IX) = UT1-SUARTVLV-PR (IX) +                
111600                                    W-SUARTVLV                            
111700     .                                                                    
111800     EJECT                                                                
111900 X-SOK-KURS SECTION.                                                      
112000                                                                          
112100     EVALUATE IN-KDVALISO (IX)                                            
112200       WHEN 'ATS'  PERFORM X-SOK-KURS-FOR-VALUTA-ATS                      
112300       WHEN 'AUD'  PERFORM X-SOK-KURS-FOR-VALUTA-AUD                      
112400       WHEN 'BEF'  PERFORM X-SOK-KURS-FOR-VALUTA-BEF                      
112410       WHEN 'BRL'  PERFORM X-SOK-KURS-FOR-VALUTA-BRL                      
112500       WHEN 'CAD'  PERFORM X-SOK-KURS-FOR-VALUTA-CAD                      
112600       WHEN 'CHF'  PERFORM X-SOK-KURS-FOR-VALUTA-CHF                      
112700       WHEN 'CLP'  PERFORM X-SOK-KURS-FOR-VALUTA-CLP                      
112710       WHEN 'CNY'  PERFORM X-SOK-KURS-FOR-VALUTA-CLP                      
112800       WHEN 'CSK'  PERFORM X-SOK-KURS-FOR-VALUTA-CSK                      
112900       WHEN 'CZK'  PERFORM X-SOK-KURS-FOR-VALUTA-CZK                      
113000       WHEN 'DEM'  PERFORM X-SOK-KURS-FOR-VALUTA-DEM                      
113100       WHEN 'DKK'  PERFORM X-SOK-KURS-FOR-VALUTA-DKK                      
113200       WHEN 'ESP'  PERFORM X-SOK-KURS-FOR-VALUTA-ESP                      
113300       WHEN 'FIM'  PERFORM X-SOK-KURS-FOR-VALUTA-FIM                      
113400       WHEN 'FRF'  PERFORM X-SOK-KURS-FOR-VALUTA-FRF                      
113500       WHEN 'GBP'  PERFORM X-SOK-KURS-FOR-VALUTA-GBP                      
113600       WHEN 'HKD'  PERFORM X-SOK-KURS-FOR-VALUTA-HKD                      
113700       WHEN 'HUF'  PERFORM X-SOK-KURS-FOR-VALUTA-HUF                      
113800       WHEN 'IEP'  PERFORM X-SOK-KURS-FOR-VALUTA-IEP                      
113900       WHEN 'INR'  PERFORM X-SOK-KURS-FOR-VALUTA-INR                      
114000       WHEN 'ITL'  PERFORM X-SOK-KURS-FOR-VALUTA-ITL                      
114100       WHEN 'JPY'  PERFORM X-SOK-KURS-FOR-VALUTA-JPY                      
114200       WHEN 'KRW'  PERFORM X-SOK-KURS-FOR-VALUTA-KRW                      
114300       WHEN 'LBP'  PERFORM X-SOK-KURS-FOR-VALUTA-LBP                      
114400       WHEN 'MXN'  PERFORM X-SOK-KURS-FOR-VALUTA-MXN                      
114400       WHEN 'ZAR'  PERFORM X-SOK-KURS-FOR-VALUTA-ZAR                      
114500       WHEN 'MYR'  PERFORM X-SOK-KURS-FOR-VALUTA-MYR                      
114600       WHEN 'NLG'  PERFORM X-SOK-KURS-FOR-VALUTA-NLG                      
114700       WHEN 'NOK'  PERFORM X-SOK-KURS-FOR-VALUTA-NOK                      
114800       WHEN 'PEI'  PERFORM X-SOK-KURS-FOR-VALUTA-PEI                      
114900       WHEN 'PLN'  PERFORM X-SOK-KURS-FOR-VALUTA-PLN                      
115000       WHEN 'PTE'  PERFORM X-SOK-KURS-FOR-VALUTA-PTE                      
115100       WHEN 'SAR'  PERFORM X-SOK-KURS-FOR-VALUTA-SAR                      
115200       WHEN 'SEK'  PERFORM X-SOK-KURS-FOR-VALUTA-SEK                      
115300       WHEN 'SGD'  PERFORM X-SOK-KURS-FOR-VALUTA-SGD                      
115400       WHEN 'SKK'  PERFORM X-SOK-KURS-FOR-VALUTA-SKK                      
115500       WHEN 'THB'  PERFORM X-SOK-KURS-FOR-VALUTA-THB                      
115600       WHEN 'TRL'  PERFORM X-SOK-KURS-FOR-VALUTA-TRL                      
115700       WHEN 'TWD'  PERFORM X-SOK-KURS-FOR-VALUTA-TWD                      
115800       WHEN 'USD'  PERFORM X-SOK-KURS-FOR-VALUTA-USD                      
115900       WHEN 'EUR'  PERFORM X-SOK-KURS-FOR-VALUTA-EUR                      
116000       WHEN 'YUN'  PERFORM X-SOK-KURS-FOR-VALUTA-YUN                      
116010       WHEN 'AED'  PERFORM X-SOK-KURS-FOR-VALUTA-AED                      
116020       WHEN 'TRY'  PERFORM X-SOK-KURS-FOR-VALUTA-TRY                      
116100                                                                          
116200       WHEN OTHER  DISPLAY ' '                                            
116300                   DISPLAY ' FÖR ARTIKEL ' IN-IDARTNR                     
116400                   DISPLAY ' INDEX       ' IX                             
116500                   DISPLAY ' SAKNAS KURS FÖR VALUTA '                     
116600                           IN-KDVALISO (IX)                               
116700                   PERFORM S99-ABEND                                      
116800                                                                          
116900     END-EVALUATE                                                         
117000     .                                                                    
117100     EJECT                                                                
117200 X-SOK-KURS-FOR-VALUTA-ATS SECTION.                                       
117300                                                                          
117400     SET ATS-IX TO 1                                                      
117500     SEARCH  ATS                                                          
117600       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
117700       WHEN (ATS-DATUM-FOM  (ATS-IX)  <= W-INLEV-DATUM) AND               
117800            (ATS-DATUM-TOM  (ATS-IX)  >= W-INLEV-DATUM)                   
117900            MOVE ATS-PRKURS (ATS-IX)  TO W-PRKURS                         
118000     END-SEARCH                                                           
118100     .                                                                    
118200     SKIP3                                                                
118300 X-SOK-KURS-FOR-VALUTA-AUD SECTION.                                       
118400                                                                          
118500     SET AUD-IX TO 1                                                      
118600     SEARCH  AUD                                                          
118700       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
118800       WHEN (AUD-DATUM-FOM  (AUD-IX)  <= W-INLEV-DATUM) AND               
118900            (AUD-DATUM-TOM  (AUD-IX)  >= W-INLEV-DATUM)                   
119000            MOVE AUD-PRKURS (AUD-IX)  TO W-PRKURS                         
119100     END-SEARCH                                                           
119200     .                                                                    
119300     SKIP3                                                                
119400 X-SOK-KURS-FOR-VALUTA-BEF SECTION.                                       
119500                                                                          
119600     SET BEF-IX TO 1                                                      
119700     SEARCH  BEF                                                          
119800       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
119900       WHEN (BEF-DATUM-FOM  (BEF-IX)  <= W-INLEV-DATUM) AND               
120000            (BEF-DATUM-TOM  (BEF-IX)  >= W-INLEV-DATUM)                   
120100            MOVE BEF-PRKURS (BEF-IX)  TO W-PRKURS                         
120200     END-SEARCH                                                           
120300     .                                                                    
120400     EJECT                                                                
120410 X-SOK-KURS-FOR-VALUTA-BRL SECTION.                                       
120420                                                                          
120430     SET BRL-IX TO 1                                                      
120440     SEARCH  BRL                                                          
120450       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
120460       WHEN (BRL-DATUM-FOM  (BRL-IX)  <= W-INLEV-DATUM) AND               
120470            (BRL-DATUM-TOM  (BRL-IX)  >= W-INLEV-DATUM)                   
120480            MOVE BRL-PRKURS (BRL-IX)  TO W-PRKURS                         
120490     END-SEARCH                                                           
120491     .                                                                    
120492     EJECT                                                                
120500 X-SOK-KURS-FOR-VALUTA-CAD SECTION.                                       
120600                                                                          
120700     SET CAD-IX TO 1                                                      
120800     SEARCH  CAD                                                          
120900       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
121000       WHEN (CAD-DATUM-FOM  (CAD-IX)  <= W-INLEV-DATUM) AND               
121100            (CAD-DATUM-TOM  (CAD-IX)  >= W-INLEV-DATUM)                   
121200            MOVE CAD-PRKURS (CAD-IX)  TO W-PRKURS                         
121300     END-SEARCH                                                           
121400     .                                                                    
121500     SKIP3                                                                
121600 X-SOK-KURS-FOR-VALUTA-CHF SECTION.                                       
121700                                                                          
121800     SET CHF-IX TO 1                                                      
121900     SEARCH  CHF                                                          
122000       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
122100       WHEN (CHF-DATUM-FOM  (CHF-IX)  <= W-INLEV-DATUM) AND               
122200            (CHF-DATUM-TOM  (CHF-IX)  >= W-INLEV-DATUM)                   
122300            MOVE CHF-PRKURS (CHF-IX)  TO W-PRKURS                         
122400     END-SEARCH                                                           
122500     .                                                                    
122600     EJECT                                                                
122700 X-SOK-KURS-FOR-VALUTA-CLP SECTION.                                       
122800                                                                          
122900     SET CLP-IX TO 1                                                      
123000     SEARCH  CLP                                                          
123100       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
123200       WHEN (CLP-DATUM-FOM  (CLP-IX)  <= W-INLEV-DATUM) AND               
123300            (CLP-DATUM-TOM  (CLP-IX)  >= W-INLEV-DATUM)                   
123400            MOVE CLP-PRKURS (CLP-IX)  TO W-PRKURS                         
123500     END-SEARCH                                                           
123600     .                                                                    
123700     SKIP3                                                                
123710 X-SOK-KURS-FOR-VALUTA-CNY SECTION.                                       
123720                                                                          
123730     SET CNY-IX TO 1                                                      
123740     SEARCH  CNY                                                          
123750       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
123760       WHEN (CNY-DATUM-FOM  (CNY-IX)  <= W-INLEV-DATUM) AND               
123770            (CNY-DATUM-TOM  (CNY-IX)  >= W-INLEV-DATUM)                   
123780            MOVE CNY-PRKURS (CNY-IX)  TO W-PRKURS                         
123790     END-SEARCH                                                           
123791     .                                                                    
123792     SKIP3                                                                
123800 X-SOK-KURS-FOR-VALUTA-CSK SECTION.                                       
123900                                                                          
124000     SET CSK-IX TO 1                                                      
124100     SEARCH  CSK                                                          
124200       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
124300       WHEN (CSK-DATUM-FOM  (CSK-IX)  <= W-INLEV-DATUM) AND               
124400            (CSK-DATUM-TOM  (CSK-IX)  >= W-INLEV-DATUM)                   
124500            MOVE CSK-PRKURS (CSK-IX)  TO W-PRKURS                         
124600     END-SEARCH                                                           
124700     .                                                                    
124800     SKIP3                                                                
124900 X-SOK-KURS-FOR-VALUTA-CZK SECTION.                                       
125000                                                                          
125100     SET CZK-IX TO 1                                                      
125200     SEARCH  CZK                                                          
125300       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
125400       WHEN (CZK-DATUM-FOM  (CZK-IX)  <= W-INLEV-DATUM) AND               
125500            (CZK-DATUM-TOM  (CZK-IX)  >= W-INLEV-DATUM)                   
125600            MOVE CZK-PRKURS (CZK-IX)  TO W-PRKURS                         
125700     END-SEARCH                                                           
125800     .                                                                    
125900     EJECT                                                                
126000 X-SOK-KURS-FOR-VALUTA-DEM SECTION.                                       
126100                                                                          
126200     SET DEM-IX TO 1                                                      
126300     SEARCH  DEM                                                          
126400       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
126500       WHEN (DEM-DATUM-FOM  (DEM-IX)  <= W-INLEV-DATUM) AND               
126600            (DEM-DATUM-TOM  (DEM-IX)  >= W-INLEV-DATUM)                   
126700            MOVE DEM-PRKURS (DEM-IX)  TO W-PRKURS                         
126800     END-SEARCH                                                           
126900     .                                                                    
127000     SKIP3                                                                
127100 X-SOK-KURS-FOR-VALUTA-DKK SECTION.                                       
127200                                                                          
127300     SET DKK-IX TO 1                                                      
127400     SEARCH  DKK                                                          
127500       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
127600       WHEN (DKK-DATUM-FOM  (DKK-IX)  <= W-INLEV-DATUM) AND               
127700            (DKK-DATUM-TOM  (DKK-IX)  >= W-INLEV-DATUM)                   
127800            MOVE DKK-PRKURS (DKK-IX)  TO W-PRKURS                         
127900     END-SEARCH                                                           
128000     .                                                                    
128100     SKIP3                                                                
128200 X-SOK-KURS-FOR-VALUTA-ESP SECTION.                                       
128300                                                                          
128400     SET ESP-IX TO 1                                                      
128500     SEARCH  ESP                                                          
128600       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
128700       WHEN (ESP-DATUM-FOM  (ESP-IX)  <= W-INLEV-DATUM) AND               
128800            (ESP-DATUM-TOM  (ESP-IX)  >= W-INLEV-DATUM)                   
128900            MOVE ESP-PRKURS (ESP-IX)  TO W-PRKURS                         
129000     END-SEARCH                                                           
129100     .                                                                    
129200     EJECT                                                                
129300 X-SOK-KURS-FOR-VALUTA-FIM SECTION.                                       
129400                                                                          
129500     SET FIM-IX TO 1                                                      
129600     SEARCH  FIM                                                          
129700       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
129800       WHEN (FIM-DATUM-FOM  (FIM-IX)  <= W-INLEV-DATUM) AND               
129900            (FIM-DATUM-TOM  (FIM-IX)  >= W-INLEV-DATUM)                   
130000            MOVE FIM-PRKURS (FIM-IX)  TO W-PRKURS                         
130100     END-SEARCH                                                           
130200     .                                                                    
130300     SKIP3                                                                
130400 X-SOK-KURS-FOR-VALUTA-FRF SECTION.                                       
130500                                                                          
130600     SET FRF-IX TO 1                                                      
130700     SEARCH  FRF                                                          
130800       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
130900       WHEN (FRF-DATUM-FOM  (FRF-IX)  <= W-INLEV-DATUM) AND               
131000            (FRF-DATUM-TOM  (FRF-IX)  >= W-INLEV-DATUM)                   
131100            MOVE FRF-PRKURS (FRF-IX)  TO W-PRKURS                         
131200     END-SEARCH                                                           
131300     .                                                                    
131400     EJECT                                                                
131500 X-SOK-KURS-FOR-VALUTA-GBP SECTION.                                       
131600                                                                          
131700     SET GBP-IX TO 1                                                      
131800     SEARCH  GBP                                                          
131900       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
132000       WHEN (GBP-DATUM-FOM  (GBP-IX)  <= W-INLEV-DATUM) AND               
132100            (GBP-DATUM-TOM  (GBP-IX)  >= W-INLEV-DATUM)                   
132200            MOVE GBP-PRKURS (GBP-IX)  TO W-PRKURS                         
132300     END-SEARCH                                                           
132400     .                                                                    
132500     SKIP3                                                                
132600 X-SOK-KURS-FOR-VALUTA-HKD SECTION.                                       
132700                                                                          
132800     SET HKD-IX TO 1                                                      
132900     SEARCH  HKD                                                          
133000       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
133100       WHEN (HKD-DATUM-FOM  (HKD-IX)  <= W-INLEV-DATUM) AND               
133200            (HKD-DATUM-TOM  (HKD-IX)  >= W-INLEV-DATUM)                   
133300            MOVE HKD-PRKURS (HKD-IX)  TO W-PRKURS                         
133400     END-SEARCH                                                           
133500     .                                                                    
133600     EJECT                                                                
133700 X-SOK-KURS-FOR-VALUTA-HUF SECTION.                                       
133800                                                                          
133900     SET HUF-IX TO 1                                                      
134000     SEARCH  HUF                                                          
134100       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
134200       WHEN (HUF-DATUM-FOM  (HUF-IX)  <= W-INLEV-DATUM) AND               
134300            (HUF-DATUM-TOM  (HUF-IX)  >= W-INLEV-DATUM)                   
134400            MOVE HUF-PRKURS (HUF-IX)  TO W-PRKURS                         
134500     END-SEARCH                                                           
134600     .                                                                    
134700     SKIP3                                                                
134800 X-SOK-KURS-FOR-VALUTA-IEP SECTION.                                       
134900                                                                          
135000     SET IEP-IX TO 1                                                      
135100     SEARCH  IEP                                                          
135200       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
135300       WHEN (IEP-DATUM-FOM  (IEP-IX)  <= W-INLEV-DATUM) AND               
135400            (IEP-DATUM-TOM  (IEP-IX)  >= W-INLEV-DATUM)                   
135500            MOVE IEP-PRKURS (IEP-IX)  TO W-PRKURS                         
135600     END-SEARCH                                                           
135700     .                                                                    
135800     EJECT                                                                
135900 X-SOK-KURS-FOR-VALUTA-INR SECTION.                                       
136000                                                                          
136100     SET INR-IX TO 1                                                      
136200     SEARCH  INR                                                          
136300       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
136400       WHEN (INR-DATUM-FOM  (INR-IX)  <= W-INLEV-DATUM) AND               
136500            (INR-DATUM-TOM  (INR-IX)  >= W-INLEV-DATUM)                   
136600            MOVE INR-PRKURS (INR-IX)  TO W-PRKURS                         
136700     END-SEARCH                                                           
136800     .                                                                    
136900     SKIP3                                                                
137000 X-SOK-KURS-FOR-VALUTA-ITL SECTION.                                       
137100                                                                          
137200     SET ITL-IX TO 1                                                      
137300     SEARCH  ITL                                                          
137400       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
137500       WHEN (ITL-DATUM-FOM  (ITL-IX)  <= W-INLEV-DATUM) AND               
137600            (ITL-DATUM-TOM  (ITL-IX)  >= W-INLEV-DATUM)                   
137700            MOVE ITL-PRKURS (ITL-IX)  TO W-PRKURS                         
137800     END-SEARCH                                                           
137900     .                                                                    
138000     EJECT                                                                
138100 X-SOK-KURS-FOR-VALUTA-MXN SECTION.                                       
138200                                                                          
138300     SET MXN-IX TO 1                                                      
138400     SEARCH  MXN                                                          
138500       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
138600       WHEN (MXN-DATUM-FOM  (MXN-IX)  <= W-INLEV-DATUM) AND               
138700            (MXN-DATUM-TOM  (MXN-IX)  >= W-INLEV-DATUM)                   
138800            MOVE MXN-PRKURS (MXN-IX)  TO W-PRKURS                         
138900     END-SEARCH                                                           
139000     .                                                                    
139100     EJECT                                                                
138100 X-SOK-KURS-FOR-VALUTA-ZAR SECTION.                                       
138200                                                                          
138300     SET ZAR-IX TO 1                                                      
138400     SEARCH  ZAR                                                          
138500       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
138600       WHEN (ZAR-DATUM-FOM  (ZAR-IX)  <= W-INLEV-DATUM) AND               
138700            (ZAR-DATUM-TOM  (ZAR-IX)  >= W-INLEV-DATUM)                   
138800            MOVE ZAR-PRKURS (ZAR-IX)  TO W-PRKURS                         
138900     END-SEARCH                                                           
139000     .                                                                    
139100     EJECT                                                                
139200 X-SOK-KURS-FOR-VALUTA-JPY SECTION.                                       
139300                                                                          
139400     SET JPY-IX TO 1                                                      
139500     SEARCH  JPY                                                          
139600       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
139700       WHEN (JPY-DATUM-FOM  (JPY-IX)  <= W-INLEV-DATUM) AND               
139800            (JPY-DATUM-TOM  (JPY-IX)  >= W-INLEV-DATUM)                   
139900            MOVE JPY-PRKURS (JPY-IX)  TO W-PRKURS                         
140000     END-SEARCH                                                           
140100     .                                                                    
140200     SKIP3                                                                
140300 X-SOK-KURS-FOR-VALUTA-KRW SECTION.                                       
140400                                                                          
140500     SET KRW-IX TO 1                                                      
140600     SEARCH  KRW                                                          
140700       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
140800       WHEN (KRW-DATUM-FOM  (KRW-IX)  <= W-INLEV-DATUM) AND               
140900            (KRW-DATUM-TOM  (KRW-IX)  >= W-INLEV-DATUM)                   
141000            MOVE KRW-PRKURS (KRW-IX)  TO W-PRKURS                         
141100     END-SEARCH                                                           
141200     .                                                                    
141300     EJECT                                                                
141400 X-SOK-KURS-FOR-VALUTA-LBP SECTION.                                       
141500                                                                          
141600     SET LBP-IX TO 1                                                      
141700     SEARCH  LBP                                                          
141800       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
141900       WHEN (LBP-DATUM-FOM  (LBP-IX)  <= W-INLEV-DATUM) AND               
142000            (LBP-DATUM-TOM  (LBP-IX)  >= W-INLEV-DATUM)                   
142100            MOVE LBP-PRKURS (LBP-IX)  TO W-PRKURS                         
142200     END-SEARCH                                                           
142300     .                                                                    
142400     SKIP3                                                                
142500 X-SOK-KURS-FOR-VALUTA-MYR SECTION.                                       
142600                                                                          
142700     SET MYR-IX TO 1                                                      
142800     SEARCH  MYR                                                          
142900       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
143000       WHEN (MYR-DATUM-FOM  (MYR-IX)  <= W-INLEV-DATUM) AND               
143100            (MYR-DATUM-TOM  (MYR-IX)  >= W-INLEV-DATUM)                   
143200            MOVE MYR-PRKURS (MYR-IX)  TO W-PRKURS                         
143300     END-SEARCH                                                           
143400     .                                                                    
143500     EJECT                                                                
143600 X-SOK-KURS-FOR-VALUTA-NLG SECTION.                                       
143700                                                                          
143800     SET NLG-IX TO 1                                                      
143900     SEARCH  NLG                                                          
144000       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
144100       WHEN (NLG-DATUM-FOM  (NLG-IX)  <= W-INLEV-DATUM) AND               
144200            (NLG-DATUM-TOM  (NLG-IX)  >= W-INLEV-DATUM)                   
144300            MOVE NLG-PRKURS (NLG-IX)  TO W-PRKURS                         
144400     END-SEARCH                                                           
144500     .                                                                    
144600     SKIP3                                                                
144700 X-SOK-KURS-FOR-VALUTA-NOK SECTION.                                       
144800                                                                          
144900     SET NOK-IX TO 1                                                      
145000     SEARCH  NOK                                                          
145100       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
145200       WHEN (NOK-DATUM-FOM  (NOK-IX)  <= W-INLEV-DATUM) AND               
145300            (NOK-DATUM-TOM  (NOK-IX)  >= W-INLEV-DATUM)                   
145400            MOVE NOK-PRKURS (NOK-IX)  TO W-PRKURS                         
145500     END-SEARCH                                                           
145600     .                                                                    
145700     EJECT                                                                
145800 X-SOK-KURS-FOR-VALUTA-PEI SECTION.                                       
145900                                                                          
146000     SET PEI-IX TO 1                                                      
146100     SEARCH  PEI                                                          
146200       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
146300       WHEN (PEI-DATUM-FOM  (PEI-IX)  <= W-INLEV-DATUM) AND               
146400            (PEI-DATUM-TOM  (PEI-IX)  >= W-INLEV-DATUM)                   
146500            MOVE PEI-PRKURS (PEI-IX)  TO W-PRKURS                         
146600     END-SEARCH                                                           
146700     .                                                                    
146800     SKIP3                                                                
146900 X-SOK-KURS-FOR-VALUTA-PLN SECTION.                                       
147000                                                                          
147100     SET PLN-IX TO 1                                                      
147200     SEARCH  PLN                                                          
147300       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
147400       WHEN (PLN-DATUM-FOM  (PLN-IX)  <= W-INLEV-DATUM) AND               
147500            (PLN-DATUM-TOM  (PLN-IX)  >= W-INLEV-DATUM)                   
147600            MOVE PLN-PRKURS (PLN-IX)  TO W-PRKURS                         
147700     END-SEARCH                                                           
147800     .                                                                    
147900     EJECT                                                                
148000 X-SOK-KURS-FOR-VALUTA-PTE SECTION.                                       
148100                                                                          
148200     SET PTE-IX TO 1                                                      
148300     SEARCH  PTE                                                          
148400       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
148500       WHEN (PTE-DATUM-FOM  (PTE-IX)  <= W-INLEV-DATUM) AND               
148600            (PTE-DATUM-TOM  (PTE-IX)  >= W-INLEV-DATUM)                   
148700            MOVE PTE-PRKURS (PTE-IX)  TO W-PRKURS                         
148800     END-SEARCH                                                           
148900     .                                                                    
149000     SKIP3                                                                
149100 X-SOK-KURS-FOR-VALUTA-SAR SECTION.                                       
149200                                                                          
149300     SET SAR-IX TO 1                                                      
149400     SEARCH  SAR                                                          
149500       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
149600       WHEN (SAR-DATUM-FOM  (SAR-IX)  <= W-INLEV-DATUM) AND               
149700            (SAR-DATUM-TOM  (SAR-IX)  >= W-INLEV-DATUM)                   
149800            MOVE SAR-PRKURS (SAR-IX)  TO W-PRKURS                         
149900     END-SEARCH                                                           
150000     .                                                                    
150100     EJECT                                                                
150200 X-SOK-KURS-FOR-VALUTA-SEK SECTION.                                       
150300                                                                          
150400     SET SEK-IX TO 1                                                      
150500     SEARCH  SEK                                                          
150600       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
150700       WHEN (SEK-DATUM-FOM  (SEK-IX)  <= W-INLEV-DATUM) AND               
150800            (SEK-DATUM-TOM  (SEK-IX)  >= W-INLEV-DATUM)                   
150900            MOVE SEK-PRKURS (SEK-IX)  TO W-PRKURS                         
151000     END-SEARCH                                                           
151100     .                                                                    
151200     SKIP3                                                                
151300 X-SOK-KURS-FOR-VALUTA-SGD SECTION.                                       
151400                                                                          
151500     SET SGD-IX TO 1                                                      
151600     SEARCH  SGD                                                          
151700       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
151800       WHEN (SGD-DATUM-FOM  (SGD-IX)  <= W-INLEV-DATUM) AND               
151900            (SGD-DATUM-TOM  (SGD-IX)  >= W-INLEV-DATUM)                   
152000            MOVE SGD-PRKURS (SGD-IX)  TO W-PRKURS                         
152100     END-SEARCH                                                           
152200     .                                                                    
152300     SKIP3                                                                
152400 X-SOK-KURS-FOR-VALUTA-SKK SECTION.                                       
152500                                                                          
152600     SET SKK-IX TO 1                                                      
152700     SEARCH  SKK                                                          
152800       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
152900       WHEN (SKK-DATUM-FOM  (SKK-IX)  <= W-INLEV-DATUM) AND               
153000            (SKK-DATUM-TOM  (SKK-IX)  >= W-INLEV-DATUM)                   
153100            MOVE SKK-PRKURS (SKK-IX)  TO W-PRKURS                         
153200     END-SEARCH                                                           
153300     .                                                                    
153400     EJECT                                                                
153500 X-SOK-KURS-FOR-VALUTA-THB SECTION.                                       
153600                                                                          
153700     SET THB-IX TO 1                                                      
153800     SEARCH  THB                                                          
153900       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
154000       WHEN (THB-DATUM-FOM  (THB-IX)  <= W-INLEV-DATUM) AND               
154100            (THB-DATUM-TOM  (THB-IX)  >= W-INLEV-DATUM)                   
154200            MOVE THB-PRKURS (THB-IX)  TO W-PRKURS                         
154300     END-SEARCH                                                           
154400     .                                                                    
154500     SKIP3                                                                
154600 X-SOK-KURS-FOR-VALUTA-TRL SECTION.                                       
154700                                                                          
154800     SET TRL-IX TO 1                                                      
154900     SEARCH  TRL                                                          
155000       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
155100       WHEN (TRL-DATUM-FOM  (TRL-IX)  <= W-INLEV-DATUM) AND               
155200            (TRL-DATUM-TOM  (TRL-IX)  >= W-INLEV-DATUM)                   
155300            MOVE TRL-PRKURS (TRL-IX)  TO W-PRKURS                         
155400     END-SEARCH                                                           
155500     .                                                                    
155600     EJECT                                                                
155700 X-SOK-KURS-FOR-VALUTA-TWD SECTION.                                       
155800                                                                          
155900     SET TWD-IX TO 1                                                      
156000     SEARCH  TWD                                                          
156100       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
156200       WHEN (TWD-DATUM-FOM  (TWD-IX)  <= W-INLEV-DATUM) AND               
156300            (TWD-DATUM-TOM  (TWD-IX)  >= W-INLEV-DATUM)                   
156400            MOVE TWD-PRKURS (TWD-IX)  TO W-PRKURS                         
156500     END-SEARCH                                                           
156600     .                                                                    
156700     SKIP3                                                                
156800 X-SOK-KURS-FOR-VALUTA-USD SECTION.                                       
156900                                                                          
157000     SET USD-IX TO 1                                                      
157100     SEARCH  USD                                                          
157200       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
157300       WHEN (USD-DATUM-FOM  (USD-IX)  <= W-INLEV-DATUM) AND               
157400            (USD-DATUM-TOM  (USD-IX)  >= W-INLEV-DATUM)                   
157500            MOVE USD-PRKURS (USD-IX)  TO W-PRKURS                         
157600     END-SEARCH                                                           
157700     .                                                                    
157800     SKIP3                                                                
157900 X-SOK-KURS-FOR-VALUTA-EUR SECTION.                                       
158000                                                                          
158100     SET EUR-IX TO 1                                                      
158200     SEARCH  EUR                                                          
158300       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
158400       WHEN (EUR-DATUM-FOM  (EUR-IX)  <= W-INLEV-DATUM) AND               
158500            (EUR-DATUM-TOM  (EUR-IX)  >= W-INLEV-DATUM)                   
158600            MOVE EUR-PRKURS (EUR-IX)  TO W-PRKURS                         
158700     END-SEARCH                                                           
158800     .                                                                    
158900     EJECT                                                                
159000 X-SOK-KURS-FOR-VALUTA-YUN SECTION.                                       
159100                                                                          
159200     SET YUN-IX TO 1                                                      
159300     SEARCH  YUN                                                          
159400       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
159500       WHEN (YUN-DATUM-FOM  (YUN-IX)  <= W-INLEV-DATUM) AND               
159600            (YUN-DATUM-TOM  (YUN-IX)  >= W-INLEV-DATUM)                   
159700            MOVE YUN-PRKURS (YUN-IX)  TO W-PRKURS                         
159800     END-SEARCH                                                           
159900     .                                                                    
160000     EJECT                                                                
160010 X-SOK-KURS-FOR-VALUTA-AED SECTION.                                       
160020                                                                          
160030     SET AED-IX TO 1                                                      
160040     SEARCH  AED                                                          
160050       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
160060       WHEN (AED-DATUM-FOM  (AED-IX)  <= W-INLEV-DATUM) AND               
160070            (AED-DATUM-TOM  (AED-IX)  >= W-INLEV-DATUM)                   
160080            MOVE AED-PRKURS (AED-IX)  TO W-PRKURS                         
160090     END-SEARCH                                                           
160091     .                                                                    
160092     EJECT                                                                
160093 X-SOK-KURS-FOR-VALUTA-TRY SECTION.                                       
160094                                                                          
160095     SET TRY-IX TO 1                                                      
160096     SEARCH  TRY                                                          
160097       AT END DISPLAY 'KURS FÖR VALUTA ' IN-KDVALISO (IX)' SAKNAS'        
160098       WHEN (TRY-DATUM-FOM  (TRY-IX)  <= W-INLEV-DATUM) AND               
160099            (TRY-DATUM-TOM  (TRY-IX)  >= W-INLEV-DATUM)                   
160100            MOVE TRY-PRKURS (TRY-IX)  TO W-PRKURS                         
160101     END-SEARCH                                                           
160102     .                                                                    
160103     EJECT                                                                
160110 Z-FINIT SECTION.                                                         
160200                                                                          
160300     CLOSE KURSFIL                                                        
160400           INFIL                                                          
160500           UTFIL1                                                         
160600           UTFIL2                                                         
160700                                                                          
160800     MOVE 'S' TO POSTSUM-OPKOD                                            
160900     CALL POSTSUM USING POSTSUM-PARM                                      
161000     .                                                                    
161100     EJECT                                                                
161200 S01-LAES-KURSFIL SECTION.                                                
161300                                                                          
161400     READ KURSFIL INTO K-AREA                                             
161500     AT END                                                               
161600        MOVE JA TO KURSFIL-EOF-SW                                         
161700     NOT AT END                                                           
161800        MOVE 'W51237'   TO POSTSUM-FDNAMN                                 
161900        MOVE 'W51238D1' TO POSTSUM-DDNAMN2                                
162000        MOVE 'KURS'     TO POSTSUM-TRANSTYP                               
162100        CALL POSTSUM USING POSTSUM-PARM                                   
162200     END-READ                                                             
162300     .                                                                    
162400     SKIP3                                                                
162500 S02-LAES-INFIL SECTION.                                                  
162600                                                                          
162700     READ INFIL                                                           
162800     AT END                                                               
162900        MOVE JA TO INFIL-EOF-SW                                           
163000     NOT AT END                                                           
163100        MOVE IN-RECORD  TO SORT-POST                                      
163200        MOVE 'W51235'   TO POSTSUM-FDNAMN                                 
163300        MOVE 'W51238D2' TO POSTSUM-DDNAMN2                                
163400        MOVE 'IN  '     TO POSTSUM-TRANSTYP                               
163500        CALL POSTSUM USING POSTSUM-PARM                                   
163600     END-READ                                                             
163700     .                                                                    
163800     SKIP3                                                                
163900 S03-LAES-SORT  SECTION.                                                  
164000                                                                          
164100     RETURN SORTFIL                                                       
164200     AT END                                                               
164300        MOVE HIGH-VALUE TO IN-W51233                                      
164400        MOVE JA TO SORTFIL-EOF-SW                                         
164500     NOT AT END                                                           
164600        MOVE SORT-POST  TO IN-AREA                                        
164700        MOVE 'SORTUT'   TO POSTSUM-FDNAMN                                 
164800        MOVE 'W51238DS' TO POSTSUM-DDNAMN2                                
164900        MOVE 'SORT'     TO POSTSUM-TRANSTYP                               
165000        CALL POSTSUM USING POSTSUM-PARM                                   
165100     END-RETURN                                                           
165200     .                                                                    
165300     EJECT                                                                
165400 S11-SKRIV-UTFIL1 SECTION.                                                
165500     SKIP2                                                                
165600     WRITE UT1-POST FROM UT1-AREA                                         
165700                                                                          
165800     MOVE 'TOT '     TO POSTSUM-TRANSTYP                                  
165900     MOVE 'W51239'   TO POSTSUM-FDNAMN                                    
166000     MOVE 'W51238D3' TO POSTSUM-DDNAMN2                                   
166100     CALL POSTSUM USING POSTSUM-PARM                                      
166200     .                                                                    
166300     SKIP3                                                                
166400 S12-SKRIV-UTFIL2 SECTION.                                                
166500     SKIP2                                                                
166600     WRITE UT2-POST FROM UT2-AREA                                         
166700                                                                          
166800     MOVE 'EJSL'     TO POSTSUM-TRANSTYP                                  
166900     MOVE 'W51239'   TO POSTSUM-FDNAMN                                    
167000     MOVE 'W51238D4' TO POSTSUM-DDNAMN2                                   
167100     CALL POSTSUM USING POSTSUM-PARM                                      
167200     .                                                                    
167300     EJECT                                                                
167400 S99-ABEND SECTION.                                                       
167500     SKIP2                                                                
167600     MOVE 'S' TO POSTSUM-OPKOD                                            
167700     CALL POSTSUM USING POSTSUM-PARM                                      
167800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
167900     .                                                                    
168000     EJECT                                                                
168100* --- IMS SEKTIONER ---                                                   
168200     SKIP3                                                                
168300 IMS-GU-WLLEVA01 SECTION.                                                 
168400                                                                          
168500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
168600     DELIMITED BY SIZE INTO SSA1                                          
168700     MOVE '  GE' TO GODK-STATUSKODER                                      
168800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
168900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169200     SKIP3                                                                
169300 IMS-GNP-WLLEVA11 SECTION.                                                
169400                                                                          
169510     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
169520     DELIMITED BY SIZE INTO SSA1                                          
169600     MOVE '  GE' TO GODK-STATUSKODER                                      
169700     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA SSA1                     
169800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
169900     PERFORM IMS-STATUSKONTROLL                                           
170000     .                                                                    
170100     EJECT                                                                
170200 IMS-GU-WLINLE01 SECTION.                                                 
170300                                                                          
170400     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
170500              DELIMITED BY SIZE INTO SSA1                                 
170600     MOVE '  GE' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA SSA1                      
170800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100     SKIP3                                                                
171200 IMS-GNP-WLINLE11 SECTION.                                                
171300                                                                          
171400     MOVE 'WLINLE11 ' TO SSA1                                             
171500     MOVE '  GE' TO GODK-STATUSKODER                                      
171600     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1                     
171700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
171800     PERFORM IMS-STATUSKONTROLL                                           
171900     .                                                                    
172000     EJECT                                                                
172100 IMS-GNP-WLINLE21 SECTION.                                                
172200                                                                          
172300     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
172400              DELIMITED BY SIZE INTO SSA1                                 
172500     MOVE 'WLINLE21 ' TO SSA2                                             
172600     MOVE '  GE' TO GODK-STATUSKODER                                      
172700     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
172800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
172900     PERFORM IMS-STATUSKONTROLL                                           
173000     .                                                                    
173100     SKIP3                                                                
173200 IMS-GNP-WLINLE22 SECTION.                                                
173300                                                                          
173400     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
173500              DELIMITED BY SIZE INTO SSA1                                 
173600     MOVE 'WLINLE22 ' TO SSA2                                             
173700     MOVE '  GE' TO GODK-STATUSKODER                                      
173800     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA SSA1 SSA2                
173900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
174000     PERFORM IMS-STATUSKONTROLL                                           
174100     .                                                                    
174200     SKIP3                                                                
174300     EJECT                                                                
174400 IMS-STATUSKONTROLL SECTION.                                              
174500     SKIP2                                                                
174600     SET STATUS-IX TO 1                                                   
174700     SEARCH GODK-STATUS                                                   
174800       AT END                                                             
174900         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
175000         DISPLAY FELTEXT                                                  
175100         CALL FELLOG                                                      
175200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
175300         CONTINUE                                                         
175400     END-SEARCH                                                           
175500     .                                                                    
175600     EJECT                                                                
175700*    -COPY  WY2000P1                                                      
