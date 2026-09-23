000100 01  RESP-WB0101O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WB0101         
000300*                                 ACCESSORIES SHOW LIST                   
000400     03 RESP-TIAOINF-AAVV-FOM-KEY                                         
000500                             PIC 9(4).                                    
000600*                                 DATUM ÄO-INFÖRANDE                      
000700*                                 CO INTRODUCTION WEEK                    
000800     03 RESP-TIAOINF-AAVV-TOM-KEY                                         
000900                             PIC 9(4).                                    
001000*                                 DATUM ÄO-INFÖRANDE                      
001100*                                 CO INTRODUCTION WEEK                    
001200     03 RESP-IDUPPDKU-KEY    PIC 9(8).                                    
001300*                                 KU-UPPDRAGSNUMMER TIKO                  
001400*                                 KU COMMISSION IDENTITY NO               
001500     03 RESP-IDUPPDSU-KEY    PIC 9(8).                                    
001600*                                 SU-UPPDRAGSNUMMER TIKO                  
001700*                                 SU COMMISSION IDENTITY NO               
001800     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100     03 RESP-BEUPPDSU-KEY    PIC X(35).                                   
002200*                                 SU-UPPDRAG BENÄMNING                    
002300*                                 SU COMMISSION DESCRIPTION               
002400     03 RESP-KVRADER-W-START PIC Z(4)9.                                   
002500*                                 ANTAL RADER                             
002600*                                 NUMBER OF LINES                         
002700     03 RESP-KVRADER-W-VISAS PIC Z(4)9.                                   
002800*                                 ANTAL RADER                             
002900*                                 NUMBER OF LINES                         
003000     03 RESP-KVRADER-W-TOTAL PIC Z(4)9.                                   
003100*                                 ANTAL RADER                             
003200*                                 NUMBER OF LINES                         
003300     03 RESP-KVRADER-D-TOTAL PIC Z(4)9.                                   
003400*                                 ANTAL RADER                             
003500*                                 NUMBER OF LINES                         
003600     03 RESP-KVRADER-D-SIST  PIC Z(4)9.                                   
003700*                                 ANTAL RADER                             
003800*                                 NUMBER OF LINES                         
003900     03 RESP-TABELLRAD       OCCURS 100 TIMES.                            
004000*                                 GRUPP MED TABELLRADER                   
004100        05 RESP-FLANNULL     PIC X.                                       
004200*                                 ANNULLATION                             
004300*                                 CANCELLATION                            
004400        05 RESP-IDARTNR      PIC Z(7)9.                                   
004500*                                 ARTIKELNUMMER                           
004600*                                 PART NUMBER                             
004700        05 RESP-KDARTTYP     PIC X.                                       
004800*                                 TYP AV ARTIKEL                          
004900*                                 TYPE OF PART                            
005000        05 RESP-TEARTUTFG    PIC X(8).                                    
005100*                                 ARTIKELUTF.GILTIGHET KDP                
005200*                                 PART DESIGN VALIDTY KDP                 
005300        05 RESP-BEART        PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500*                                 PART DESCRIPTION                        
005600        05 RESP-TENOTE       PIC X(40).                                   
005700*                                 NOTERINGSFÄLT                           
005800*                                 NOTE FIELD                              
005900        05 RESP-IDPRODGR     PIC X(4).                                    
006000*                                 PRODUKTGRUPP CHEF                       
006100*                                 PRODUCT GROUP MANAGER                   
006200        05 RESP-IDUPPDKU     PIC X(8).                                    
006300*                                 KU-UPPDRAGSNUMMER TIKO                  
006400*                                 KU COMMISSION IDENTITY NO               
006500        05 RESP-IDUPPDSU     PIC X(8).                                    
006600*                                 SU-UPPDRAGSNUMMER TIKO                  
006700*                                 SU COMMISSION IDENTITY NO               
006800        05 RESP-TESTATUPP    PIC X(6).                                    
006900*                                 UPPDRAGSNUMMER STATUS                   
007000*                                 ASSIGNMENT NO. STATUS                   
007100        05 RESP-IDAOT        PIC X(6).                                    
007200*                                 ÄNDRINGSORDERNUMMER T                   
007300*                                 CONSTRUCTION ORDER T                    
007400        05 RESP-TIAOINF-AAVV PIC Z(4).                                    
007500*                                 DATUM ÄO-INFÖRANDE                      
007600*                                 CO INTRODUCTION WEEK                    
007700        05 RESP-BEASSTYP     PIC X(7).                                    
007800*                                 TILLBEHÖR TILLDELNINGSTYP               
007900*                                 ACCESSORY ASSIGNMENT TYPE               
008000        05 RESP-IDANSK       PIC Z(2)9.                                   
008100*                                 ANSKAFFARNUMMER                         
008200*                                 PROCURER NO.                            
008300        05 RESP-FLRPULS      PIC X.                                       
008400*                                 ARTIKEL REG PÅ ARTREG                   
008500*                                 PARTNO REG IN PULS                      
008600        05 RESP-BETEXT-OTP   PIC X(5).                                    
008700*                                 INKÖPSORDER UTFÄRDAD, TILLB             
008800*                                 ORDER TO PURCHASE, ACCESORIES           
008900        05 RESP-TIAVTAL      PIC Z(4)9.                                   
009000*                                 ÅR - VECKA  (ÅÅVV)                      
009100*                                 YEAR - WEEK  (YYWW)                     
009200        05 RESP-IDINK        PIC X(4).                                    
009300*                                 INKÖPARNUMMER                           
009400*                                 PURCHASE IDENTIFICATION NUMBER          
009500        05 RESP-IDSTEKN      PIC X(8).                                    
009600*                                 SITE TEKNIKER                           
009700*                                 QA TECHNICIAN                           
009800        05 RESP-IDLEVNR      PIC X(5).                                    
009900*                                 LEVERANTÖRNUMMER                        
010000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
010100        05 RESP-KDLEVPST     PIC X.                                       
010200*                                 LEVERANSPLAN STATUS, TILLB.             
010300*                                 DELIVERY SCHEDULE STATUS, ACC.          
010400        05 RESP-KDFRPTYP     PIC X.                                       
010500*                                 TYP AV FÖRPACKNING                      
010600*                                 TYPE OF PACKAGE                         
010700        05 RESP-KDEMBKOD-2   PIC Z(2)9.                                   
010800*                                 EMBALLAGEKOD 2                          
010900        05 RESP-TITPD-AAVV   PIC Z(4).                                    
011000*                                 VECKA TILLFÄLLIGT UTFALLSPROV           
011100*                                 TEMP PRODUCTION DEVIATION WEEK          
011200        05 RESP-KDTPD        PIC X.                                       
011300*                                 KOD TILLFÄLLIGT UTFALLSPROV             
011400*                                 TEMP PRODUCION DEVIATION CODE           
011500        05 RESP-KDMDS        PIC X.                                       
011600*                                 MATERIALDATAKOD                         
011700*                                 MATERIAL DATA CODE                      
011800        05 RESP-FLTPDWKPH1   PIC X.                                       
011900*                                 ALLMÄN FLAGGA                           
012000*                                 GENERAL FLAG                            
012100        05 RESP-DAPSWQA-1    PIC 9(6).                                    
012200*                                 PSW-A-QUAL-PHASE1(AAAAVV)               
012300*                                 PSW-A-QUAL-PHASE1(YYYYWW)               
012400        05 RESP-KDPSWQA-1    PIC X.                                       
012500*                                 STATUSKOD AKT QUAL PH1                  
012600*                                 STATUS CODE ACT QUAL PH1                
012700        05 RESP-DAPSWPA-2    PIC 9(6).                                    
012800*                                 PSW-A-PROD-PHASE2(AAAAVV)               
012900*                                 PSW-A-PROD-PHASE2(YYYYWW)               
013000        05 RESP-KDPSWPA-2    PIC X.                                       
013100*                                 STATUSKOD AKT PROD PH2                  
013200*                                 STATUS CODE ACT PROD PH2                
013300        05 RESP-DAPSWCA-3    PIC 9(6).                                    
013400*                                 PSW-A-CAP-PHASE3 (AAAAVV)               
013500*                                 PSW-A-CAP-PHASE3 (YYYYWW)               
013600        05 RESP-KDPSWCA-3    PIC X.                                       
013700*                                 STATUSKOD AKT CAP PH3                   
013800*                                 STATUS CODE ACT CAP PH3                 
013900        05 RESP-KVLS         PIC -(7)9.                                   
014000*                                 LAGERSALDO                              
014100*                                 STOCK BALANCE                           
014200        05 RESP-KVYVOL-INT   PIC Z(5)9.                                   
014300*                                 BESL. ÅRSVOL FÖR INTROD.                
014400*                                 DECIDED YR VOL TO LAUNCH                
014500        05 RESP-KVYVOL-B3    PIC Z(5)9.                                   
014600*                                 JUST.AV ÅRSVOLYM BOARD 3                
014700*                                 ADJ.OF YEAR VOLUME BOARD 3              
014800        05 RESP-KVYVOL-B2    PIC Z(5)9.                                   
014900*                                 JUST.AV ÅRSVOLYM BOARD 2                
015000*                                 ADJ. OF YEAR VOLUME BOARD 2             
015100        05 RESP-KVYVOL-B1    PIC Z(5)9.                                   
015200*                                 JUSTERAD ÅRSVOLYM BOARD 1               
015300*                                 ADJUSTED YEAR VOLUME BOARD 1            
015400        05 RESP-KVYVOL-ASS   PIC Z(5)9.                                   
015500*                                 ÅSATT ÅRSVOLYM I LAGER                  
015600*                                 ASSIGNED YEAR VOLUME                    
015700        05 RESP-BEMAPP       PIC X(5).                                    
015800*                                 MAPP                                    
015900*                                 BINDER                                  
016000        05 RESP-KVFOTO       PIC Z9.                                      
016100*                                 ANTAL FOTOGRAFIER                       
016200*                                 NUMBER OF PHOTOS                        
016300        05 RESP-TIFOTO       PIC Z(4).                                    
016400*                                 FOTO MTRL VECKA                         
016500*                                 PHOTO MTRL WEEK                         
016600        05 RESP-TEVERKTYG    PIC X(4).                                    
016700*                                 VERKTYGSKÖP INFO                        
016800        05 RESP-TESTATXT     PIC X(25).                                   
016900*                                 STA INFORMATION                         
017000        05 RESP-TEMATXT      PIC X(25).                                   
017100*                                 MA INFORMATION                          
017200        05 RESP-TEINKTXT     PIC X(25).                                   
017300*                                 INK INFORMATION                         
017400        05 RESP-TEANSTXT     PIC X(25).                                   
017500*                                 ANSK INFORMATION                        
017600        05 RESP-TEAUXTXT     PIC X(25).                                   
017700*                                 EXTRA NOTERING                          
017800        05 RESP-IDARTNR-OFARG                                             
017900                             PIC Z(7)9.                                   
018000*                                 OFARGAD ARTIKEL KDP                     
018100        05 RESP-IDPSLAG      PIC X(2).                                    
018200*                                 PSLAG IDENTITET KDP                     
018300*                                 PSLAG INTENTITY KDP                     
018400        05 RESP-BETYP        PIC X(8).                                    
018500*                                 KDP TYP                                 
018600*                                 KDP TYPE                                
018700        05 RESP-IDFKNGRP     PIC Z(3)9.                                   
018800*                                 FUNKTIONSGRUPP                          
018900*                                 FUNCTION GROUP                          
019000        05 RESP-IDKDPPOS     PIC 9(3).                                    
019100*                                 ARTIKELPOS I KDP                        
019200*                                 PART GROUP POS IN KDP                   
019300        05 RESP-IDAOTUTG     PIC 9(2).                                    
019400*                                 ÄNDRINGSORDER-T UTGÅVA                  
019500*                                 CONSTRUCT.ORDER -T ISSUE                
019600        05 RESP-BEANST-KU    PIC X(25).                                   
019700*                                 KU-ANSVARIGS NAMN                       
019800*                                 NAME OF KU RESPONSIBLE                  
019900        05 RESP-BEANST-SU    PIC X(25).                                   
020000*                                 SU-ANSVARIGS NAMN                       
020100*                                 NAME OF SU RESPONSIBLE                  
020200        05 RESP-BEUPPDSU     PIC X(35).                                   
020300*                                 SU-UPPDRAG BENÄMNING                    
020400*                                 SU COMMISSION DESCRIPTION               
020500        05 RESP-IDPSS        PIC X(5).                                    
020600*                                 PSS IDENTITET KDP                       
020700*                                 PSS INTENTITY KDP                       
020800        05 RESP-DAPSWQP-1    PIC 9(6).                                    
020900*                                 PSW-P-QUAL-PHASE1(AAAAVV)               
021000*                                 PSW-P-QUAL-PHASE1(YYYYWW)               
021100        05 RESP-KDPSWQP-1    PIC X.                                       
021200*                                 STATUSKOD PLAN QUAL PH1                 
021300*                                 STATUS CODE PLAN QUAL PH1               
021400        05 RESP-DAPSWPP-2    PIC 9(6).                                    
021500*                                 PSW-P-PROD-PHASE2(AAAAVV)               
021600*                                 PSW-P-PROD-PHASE2(YYYYWW)               
021700        05 RESP-KDPSWPP-2    PIC X.                                       
021800*                                 STATUSKOD PLAN PROD PH2                 
021900*                                 STATUS CODE PLAN PROD PH2               
022000        05 RESP-DAPSWCP-3    PIC 9(6).                                    
022100*                                 PSW-P-CAP-PHASE3 (AAAAVV)               
022200*                                 PSW-P-CAP-PHASE3 (YYYYWW)               
022300        05 RESP-KDPSWCP-3    PIC X.                                       
022400*                                 STATUSKOD PLAN CAP PH3                  
022500*                                 STATUS CODE PLAN CAP PH3                
022600        05 RESP-KDFARGST     PIC X.                                       
022700*                                 FÄRGSTATUS                              
022800*                                 COLOUR STATUS                           
022900        05 RESP-IDPROJK      PIC X(4).                                    
023000*                                 PROJEKTIDENTITET KONSTRUKTION           
023100*                                 PROJECT IDENTITY KONSTRUCTION           
023200        05 RESP-VKART-KDP    PIC 9(8).                                    
023300*                                 ARTIKELVIKT KDP (G)                     
023400*                                 PART WEIGHT KDP (G)                     
023500        05 RESP-KDANNULL     PIC X.                                       
023600*                                 CANCELLATION CODE                       
023700*                                 CANCELLATION CODE                       
023800        05 RESP-TILEVBSK     PIC 9(7).                                    
023900*                                 LEVERANSBESKEDSVECKA   (YYYYWWD         
024000*                                 )                                       
024100        05 RESP-FLPULSPR     PIC X.                                       
024200*                                 FLAGGA OM DET FINNS ETT PULS PR         
024300*                                 IS                                      
024400*                                 FLAG TO TELL IF THERE EXIST A P         
024500*                                 ULS PRICE                               
024600        05 RESP-TIAVIDAT     PIC 9(7).                                    
024700*                                 AVISERINGSDATUM (YYYYWWD)               
024800*                                 ADVICE NOTE DATE                        
024900        05 RESP-SULEVANT     PIC Z(8)9.                                   
025000*                                 SUMMA LEVERERAT ANTAL                   
025100*                                 AV 1 ARTIKEL                            
025200*                                 SUMMARY DELIVERED OF AN ITEM            
025300        05 RESP-KDSIGN       PIC X.                                       
025400*                                 DATA SIGN                               
025500*                                 DATA SIGN                               
025600*** END OF VILMAII-COPY LENGTH= 54592 BYTES                               
