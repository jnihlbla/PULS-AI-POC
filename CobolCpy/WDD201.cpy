000100 01  ART-WDD201.                                                          
000200*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000300*                                 ARTIKELINFORMATION                      
000400*                                 FYSISK NYCKEL: IDARTNR                  
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ART-BEART-SVE        PIC X(25).                                   
000900*                                 SVENSK ARTIKELBENÄMNING                 
001000     03 ART-FLAENDR          PIC X.                                       
001100*                                 ÄNDRINGSFLAGGA                          
001200*                                 MARK OF CHANGES                         
001300     03 ART-FLBASL           PIC X.                                       
001400*                                 BASLAGER MARKNADSKÖ FLAGGA              
001500*                                 BASIC STOCK MARKET QUEUE FLAG           
001600     03 ART-FLBERQ           PIC X.                                       
001700*                                 FLAGGA FÖR BEREDAR KÖ                   
001800*                                 PARTS PLANING QUEUE FLAG                
001900     03 ART-FLBYTES          PIC X.                                       
002000*                                 BYTESARTIKEL                            
002100*                                 EXCHANGE PART                           
002200     03 ART-FLPISK           PIC X.                                       
002300*                                 PISK ARTIKEL                            
002400*                                 FAST PART (PISK)                        
002500     03 ART-FLRITB           PIC X.                                       
002600*                                 KLARMARKERING B-RITNING                 
002700*                                 COMPLITION FLAG B-DRAWING               
002800     03 ART-FLRITC           PIC X.                                       
002900*                                 KLARMARKERING C-RITNING                 
003000*                                 COMPLITION FLAG C-DRAWING               
003100     03 ART-FLRITP           PIC X.                                       
003200*                                 KLARMARKERING P-RITNING                 
003300*                                 COMPLITION FLAG P-DRAWING               
003400     03 ART-FLUNIKRD         PIC X.                                       
003500*                                 FLAGGA UNIK RESERVDEL                   
003600*                                 UNIK SPARE PART FLAG                    
003700     03 ART-FLUPG            PIC X.                                       
003800*                                 FLAGGA UTFALLSPROV GODKÄNT              
003900*                                 QUALITY TEST ACCEPTED FLAG              
004000     03 ART-IDANSK           PIC S9(3)           COMP-3.                  
004100*                                 ANSKAFFARNUMMER                         
004200*                                 PROCURER NO.                            
004300     03 ART-IDANSK-REG       PIC S9(3)           COMP-3.                  
004400*                                 REGISTRERAD AV ANSKAFFARENUMMER         
004500*                                 REGISTRATED BY PROCURER NO.             
004600     03 ART-IDAO             PIC X(10).                                   
004700*                                 ÄNDRINGSORDERNUMMER                     
004800*                                 DESIGN CHANGE NOTICE                    
004900     03 ART-IDARTNR-MOTSV    PIC S9(9)           COMP-3.                  
005000*                                 MOTSVARANDE ARTIKEL                     
005100*                                 CORRESPONDING PART NO                   
005200     03 ART-IDAVD            PIC S9(5)           COMP-3.                  
005300*                                 DEN ANSTÄLLDES AVDELNING/               
005400*                                 KOSTNADSSTÄLLE                          
005500*                                 DEPARTMENT OF EMPLOYED/                 
005600*                                 COST CENTER                             
005700     03 ART-IDBERED          PIC S9(3)           COMP-3.                  
005800*                                 BEREDARENUMMER                          
005900     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
006000*                                 FUNKTIONSGRUPP                          
006100*                                 FUNCTION GROUP                          
006200     03 FILLER               PIC X(2).                                    
006300     03 ART-IDLEVNR          PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006600     03 ART-IDLEVNR-FORB     OCCURS 5 TIMES                               
006700                             PIC X(5).                                    
006800*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
006900*                                 SUPPLIER CONSUMER                       
007000     03 ART-IDMATKTO         PIC X(8).                                    
007100*                                 MATRIALKONTO, ANALYSNUMMER              
007200*                                 MATERIAL ACCOUNT, ANALYSE NO            
007300     03 ART-IDPROENH         PIC X(8).                                    
007400*                                 PRODUKTIONSENHET                        
007500*                                 PRODUCTION UNIT                         
007600     03 ART-IDPROJ           PIC X(4).                                    
007700*                                 PARTS PROJEKTIDENTITET                  
007800*                                 PARTS PROJECT IDENTITY                  
007900     03 ART-IDPROJK          PIC X(4).                                    
008000*                                 PROJEKTIDENTITET KONSTRUKTION           
008100*                                 PROJECT IDENTITY KONSTRUCTION           
008200     03 ART-IDPROJOBJ        PIC X(4).                                    
008300*                                 PROJEKTIDENTITET LV OBJEKT              
008400*                                 PROJECT IDENTITY, TRUCK OBJECT          
008500     03 ART-IDRITN           PIC X(10).                                   
008600*                                 RITNINGSNUMMER                          
008700*                                 DRAWING NUMBER                          
008800     03 ART-IDRITUTG         PIC X(3).                                    
008900*                                 RITNINGSUTGÅVA                          
009000*                                 DRAWING EDITION (IMAGE)                 
009100     03 ART-KDANSKQ          PIC X.                                       
009200*                                 KOD FÖR ANSKAFFARE KÖ                   
009300*                                 PROCURER QUEUE CODE                     
009400     03 ART-KDARTTYP         PIC X.                                       
009500*                                 TYP AV ARTIKEL                          
009600*                                 TYPE OF PART                            
009700     03 ART-KDARTUTG         PIC X.                                       
009800*                                 KOD ARTIKELN UTGÅR                      
009900*                                 PART DELETE CODE                        
010000     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
010100*                                 PRODUKTSLAG                             
010200*                                 PRODUCT GROUP                           
010300     03 ART-KDRESBED         PIC X.                                       
010400*                                 RESERVDELSBEDÖMNINGSKOD                 
010500*                                 SPARE PART JUDGEMENT CODE               
010600     03 ART-KDSORT           PIC X(2).                                    
010700*                                 SORT-KOD                                
010800*                                 UNIT OF MEASURE                         
010900     03 ART-KDSTAINK         PIC S9              COMP-3.                  
011000*                                 STATUS PRIS FRÅN INKÖP                  
011100*                                 STATUS CODE FROM PURCH. DEPARTM         
011200     03 ART-KVARTAR1         PIC S9(9)           COMP-3.                  
011300*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 1         
011400*                                 QUANT PARTS CONSUMPTION YEAR 1          
011500     03 ART-KVARTAR2         PIC S9(9)           COMP-3.                  
011600*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 2         
011700*                                 QUANT PARTS CONSUMPTION YEAR 2          
011800     03 ART-KVARTAR3         PIC S9(9)           COMP-3.                  
011900*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 3         
012000*                                 QUANT PARTS CONSUMPTION YEAR 3          
012100     03 ART-KVARTVAGN        PIC S9(3)           COMP-3.                  
012200*                                 ANTAL ARTIKLAR PER VAGN                 
012300*                                 QUANT PARTS PER VEHICLE                 
012400     03 ART-KVBASL           PIC S9(7)           COMP-3.                  
012500*                                 BASLAGER TOTAL PER ARTIKEL              
012600*                                 BASIC STOCK PER PART                    
012700     03 ART-KVLEVBEG         PIC S9(7)           COMP-3.                  
012800*                                 BEGÄRT ANTAL ATT LEVERERAS              
012900*                                 QUANTITY TO BE DELIVERED                
013000     03 ART-KDKOPTYP         PIC X.                                       
013100*                                 KOD TYP AV INKÖP                        
013200*                                 CODE FOR TYPE OF PURCHASE               
013300     03 FILLER               PIC X(3).                                    
013400     03 ART-KVPROG           PIC S9(7)           COMP-3.                  
013500*                                 ÅRSPROGNOS                              
013600*                                 PROGNOS OF THE YEAR                     
013700     03 ART-KVUPB            PIC S9(5)           COMP-3.                  
013800*                                 ANTAL SOM SKALL UTFALLSPROVAS           
013900*                                 QUANTITY FOR QUALITY CONTROL            
014000     03 ART-PRARTBES         PIC S9(7)V9(2)      COMP-3.                  
014100*                                 BESTÄLLNINGSPRIS I KRONOR               
014200*                                 ORDER PRICE SWEDISH CURRENCY            
014300     03 ART-TEANSINK         PIC X(50).                                   
014400*                                 NOTERING VID FÖRSTA KÖP                 
014500*                                 PROCURER NOTE FIRST PURCHASE            
014600     03 ART-TEARTNOT         PIC X(40).                                   
014700*                                 ARTIKEL NOTERING                        
014800*                                 PART REMARKS NOTE                       
014900     03 ART-TEARTNOT-BASL    PIC X(40).                                   
015000*                                 BASLAGER ARTIKEL NOTERING               
015100*                                 PARTS NOTIFY BASIC STOCK                
015200     03 ART-TEORSAK          PIC X(50).                                   
015300*                                 INFO OM SLAG AV ÅTGÄRD                  
015400*                                 REASON INFORMATION                      
015500     03 ART-TETEKNIK         PIC X(50).                                   
015600*                                 TEKNISK INFO FRÅN PV/LV                 
015700*                                 TECNICAL INFO FROM CARS/TRUCKS          
015800     03 ART-TIANSKREG        PIC S9(7)           COMP-3.                  
015900*                                 REGISTRERINGSDATUM ANSKAFFNING          
016000*                                 REGISTRATION DATE PROCURER              
016100     03 ART-DABASL           PIC 9(8).                                    
016200*                                 BASLAGER MARKNADS KNYTTID               
016300*                                 BASIC STOCK CONNECT TIME                
016400     03 ART-DAFINLEV         PIC 9(8).                                    
016500*                                 PUBLICERINGSDATUM  (AAAAMMDD)           
016600*                                 DATE 1:ST GOODS REC (YYYYMMDD)          
016700     03 ART-TIINKOP          PIC S9(7)           COMP-3.                  
016800*                                 DATUM NÄR INKÖP BEGÄRS                  
016900*                                 DATE FOR PURCHASE REQUEST               
017000     03 ART-TILEVBEG         PIC S9(7)           COMP-3.                  
017100*                                 DATUM NÄR LEVERANS BEGÄRS               
017200*                                 DATE FOR DELIVER REQUEST                
017300     03 ART-TINEDBRY         PIC S9(7)           COMP-3.                  
017400*                                 SLUTTID NEDBRYTNING                     
017500*                                 FINISH-TIME ASSEMBLY PLANNING           
017600     03 ART-TIPLAKOP         PIC S9(7)           COMP-3.                  
017700*                                 PLANERAD KÖPTID                         
017800*                                 PLANNED PURCHASE TIME                   
017900     03 ART-TIREGDAT         PIC S9(7)           COMP-3.                  
018000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
018100*                                 REGISTRATION DATE (YYMMDD)              
018200     03 ART-TIRITB           PIC S9(7)           COMP-3.                  
018300*                                 B-RITNINGSTID                           
018400*                                 B-DRAWING TIME                          
018500     03 ART-TIRITC           PIC S9(7)           COMP-3.                  
018600*                                 C-RITNINGSTID                           
018700*                                 C-DRAWING TIME                          
018800     03 ART-TIRITP           PIC S9(7)           COMP-3.                  
018900*                                 P-RITNINGSTID                           
019000*                                 P-DRAWING TIME                          
019100     03 ART-TISERLEV         OCCURS 5 TIMES                               
019200                             PIC S9(7)           COMP-3.                  
019300*                                 SERIELEVERANS START                     
019400*                                 SERIAL DELIVERY START                   
019500     03 ART-TISLUBER         PIC S9(7)           COMP-3.                  
019600*                                 BEREDNINGS SLUT                         
019700*                                 STOP-TIME PARTS PLANNING                
019800     03 ART-TISTABER         PIC S9(7)           COMP-3.                  
019900*                                 BEREDNINGS START                        
020000*                                 START-TIME PARTS PLANNING               
020100     03 ART-TISTOMREG        PIC S9(7)           COMP-3.                  
020200*                                 STOPPTID MARKNADSREGISTRERING           
020300*                                 STOP-TIME MARKET REGISTRATION           
020400     03 ART-TIUPB            PIC S9(7)           COMP-3.                  
020500*                                 TID NÄR UTFALLSPROV BEGÄRTS             
020600*                                 TIME FOR QUALITY TEST                   
020700     03 ART-TIUPG            PIC S9(7)           COMP-3.                  
020800*                                 TID NÄR UTFALLSPROV GJORTS              
020900*                                 DATE FOR ENDED QUALITY CONTROL          
021000     03 ART-TIUPPDAT         PIC S9(7)           COMP-3.                  
021100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
021200*                                 UPDATING DATE     (YYMMDD)              
021300     03 ART-FLUPB            PIC X.                                       
021400*                                 FLAGGA UTFALLSPROV UTFÖRT               
021500*                                 QUALITY TEST PERFORMED FLAG             
021600     03 ART-IDINKTEK         PIC S9(5)           COMP-3.                  
021700*                                 INKÖPSTEKNIKER                          
021800*                                 PURCHASE TECHNICIAN                     
021900     03 ART-FLPLAKOP         PIC X.                                       
022000*                                 ARTIKEL INKÖPT FLAGGA                   
022100*                                 PART PURCHASED FLAG                     
022200     03 ART-IDINK            PIC X(4).                                    
022300*                                 INKÖPARNUMMER                           
022400*                                 PURCHASE IDENTIFICATION NUMBER          
022500     03 ART-IDSTEKN          PIC X(8).                                    
022600*                                 SITE TEKNIKER                           
022700*                                 QA TECHNICIAN                           
022800     03 ART-KDTPD            PIC X.                                       
022900*                                 KOD TILLFÄLLIGT UTFALLSPROV             
023000*                                 TEMPORARY PRODUCION DEVIATION C         
023100*                                 ODE                                     
023200     03 ART-TITPD            PIC S9(7)           COMP-3.                  
023300*                                 TID TILLFÄLLIGT UTFALLSPROV             
023400*                                 DATE FOR TEMPORARY PRODUCTION D         
023500*                                 EVIATION                                
023600     03 ART-TIMOTSI          PIC S9(7)           COMP-3.                  
023700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
023800*                                 YEAR - MONTH - DAY  (YYMMDD)            
023900     03 FILLER               PIC X(8).                                    
024000*** END OF VILMAII-COPY LENGTH= 550 BYTES                                 
