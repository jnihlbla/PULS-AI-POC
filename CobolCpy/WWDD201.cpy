000100 01  ART-WWDD201.                                                         
000200*                                 COPYTEXT TILL W.NYPONP.WWDD2            
000300*                                 SEKVENS-KOPIA AV WDD2                   
000400*                                 SORTAT PÅ IDARTNR    PTYP = 1           
000500     03 ART-POSTTYP          PIC X.                                       
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 ART-BEART-SVE        PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 ART-FLAENDR          PIC X.                                       
001200*                                 ÄNDRINGSFLAGGA                          
001300*                                 MARK OF CHANGES                         
001400     03 ART-FLBASL           PIC X.                                       
001500*                                 BASLAGER MARKNADSKÖ FLAGGA              
001600*                                 BASIC STOCK MARKET QUEUE FLAG           
001700     03 ART-FLBERQ           PIC X.                                       
001800*                                 FLAGGA FÖR BEREDAR KÖ                   
001900*                                 PARTS PLANING QUEUE FLAG                
002000     03 ART-FLBYTES          PIC X.                                       
002100*                                 BYTESARTIKEL                            
002200*                                 EXCHANGE PART                           
002300     03 ART-FLPISK           PIC X.                                       
002400*                                 PISK ARTIKEL                            
002500*                                 FAST PART (PISK)                        
002600     03 ART-FLRITB           PIC X.                                       
002700*                                 KLARMARKERING B-RITNING                 
002800*                                 COMPLITION FLAG B-DRAWING               
002900     03 ART-FLRITC           PIC X.                                       
003000*                                 KLARMARKERING C-RITNING                 
003100*                                 COMPLITION FLAG C-DRAWING               
003200     03 ART-FLRITP           PIC X.                                       
003300*                                 KLARMARKERING P-RITNING                 
003400*                                 COMPLITION FLAG P-DRAWING               
003500     03 ART-FLUNIKRD         PIC X.                                       
003600*                                 FLAGGA UNIK RESERVDEL                   
003700*                                 UNIK SPARE PART FLAG                    
003800     03 ART-FLUPG            PIC X.                                       
003900*                                 FLAGGA UTFALLSPROV GODKÄNT              
004000*                                 QUALITY TEST ACCEPTED FLAG              
004100     03 ART-IDANSK           PIC S9(3)           COMP-3.                  
004200*                                 ANSKAFFARNUMMER                         
004300*                                 PROCURER NO.                            
004400     03 ART-IDANSK-REG       PIC S9(3)           COMP-3.                  
004500*                                 REGISTRERAD AV ANSKAFFARENUMMER         
004600*                                 REGISTRATED BY PROCURER NO.             
004700     03 ART-IDAO             PIC X(10).                                   
004800*                                 ÄNDRINGSORDERNUMMER                     
004900*                                 DESIGN CHANGE NOTICE                    
005000     03 ART-IDARTNR-MOTSV    PIC S9(9)           COMP-3.                  
005100*                                 MOTSVARANDE ARTIKEL                     
005200*                                 CORRESPONDING PART NO                   
005300     03 ART-IDAVD            PIC S9(5)           COMP-3.                  
005400*                                 DEN ANSTÄLLDES AVDELNING/               
005500*                                 KOSTNADSSTÄLLE                          
005600*                                 DEPARTMENT OF EMPLOYED/                 
005700*                                 COST CENTER                             
005800     03 ART-IDBERED          PIC S9(3)           COMP-3.                  
005900*                                 BEREDARENUMMER                          
006000     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
006100*                                 FUNKTIONSGRUPP                          
006200*                                 FUNCTION GROUP                          
006300     03 ART-IDINK            PIC S9(3)           COMP-3.                  
006400*                                 INKÖPARNUMMER                           
006500*                                 PURCHASE IDENTIFICATION NUMBER          
006600     03 ART-IDLEVNR          PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006900     03 ART-IDLEVNR-FORB     OCCURS 5 TIMES                               
007000                             PIC X(5).                                    
007100*                                 LEVERANTÖR SOM ÄR FÖRBRUKARE            
007200*                                 SUPPLIER CONSUMER                       
007300     03 ART-IDMATKTO         PIC X(8).                                    
007400*                                 MATRIALKONTO, ANALYSNUMMER              
007500*                                 MATERIAL ACCOUNT, ANALYSE NO            
007600     03 ART-IDPROENH         PIC X(8).                                    
007700*                                 PRODUKTIONSENHET                        
007800*                                 PRODUCTION UNIT                         
007900     03 ART-IDPROJ           PIC X(4).                                    
008000*                                 PARTS PROJEKTIDENTITET                  
008100*                                 PARTS PROJECT IDENTITY                  
008200     03 ART-IDPROJK          PIC X(4).                                    
008300*                                 PROJEKTIDENTITET KONSTRUKTION           
008400*                                 PROJECT IDENTITY KONSTRUCTION           
008500     03 ART-IDPROJOBJ        PIC X(4).                                    
008600*                                 PROJEKTIDENTITET LV OBJEKT              
008700*                                 PROJECT IDENTITY, TRUCK OBJECT          
008800     03 ART-IDRITN           PIC X(10).                                   
008900*                                 RITNINGSNUMMER                          
009000*                                 DRAWING NUMBER                          
009100     03 ART-IDRITUTG         PIC X(3).                                    
009200*                                 RITNINGSUTGÅVA                          
009300*                                 DRAWING EDITION (IMAGE)                 
009400     03 ART-KDANSKQ          PIC X.                                       
009500*                                 KOD FÖR ANSKAFFARE KÖ                   
009600*                                 PROCURER QUEUE CODE                     
009700     03 ART-KDARTTYP         PIC X.                                       
009800*                                 TYP AV ARTIKEL                          
009900*                                 TYPE OF PART                            
010000     03 ART-KDARTUTG         PIC X.                                       
010100*                                 KOD ARTIKELN UTGÅR                      
010200*                                 PART DELETE CODE                        
010300     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
010400*                                 PRODUKTSLAG                             
010500*                                 PRODUCT GROUP                           
010600     03 ART-KDRESBED         PIC X.                                       
010700*                                 RESERVDELSBEDÖMNINGSKOD                 
010800*                                 SPARE PART JUDGEMENT CODE               
010900     03 ART-KDSORT           PIC X(2).                                    
011000*                                 SORT-KOD                                
011100*                                 UNIT OF MEASURE                         
011200     03 ART-KDSTAINK         PIC S9              COMP-3.                  
011300*                                 STATUS PRIS FRÅN INKÖP                  
011400*                                 STATUS CODE FROM PURCH. DEPARTM         
011500     03 ART-KVARTAR1         PIC S9(9)           COMP-3.                  
011600*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 1         
011700*                                 QUANT PARTS CONSUMPTION YEAR 1          
011800     03 ART-KVARTAR2         PIC S9(9)           COMP-3.                  
011900*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 2         
012000*                                 QUANT PARTS CONSUMPTION YEAR 2          
012100     03 ART-KVARTAR3         PIC S9(9)           COMP-3.                  
012200*                                 ANTAL ARTIKLAR FÖRBRUKNING ÅR 3         
012300*                                 QUANT PARTS CONSUMPTION YEAR 3          
012400     03 ART-KVARTVAGN        PIC S9(3)           COMP-3.                  
012500*                                 ANTAL ARTIKLAR PER VAGN                 
012600*                                 QUANT PARTS PER VEHICLE                 
012700     03 ART-KVBASL           PIC S9(7)           COMP-3.                  
012800*                                 BASLAGER TOTAL PER ARTIKEL              
012900*                                 BASIC STOCK PER PART                    
013000     03 ART-KVLEVBEG         PIC S9(7)           COMP-3.                  
013100*                                 BEGÄRT ANTAL ATT LEVERERAS              
013200*                                 QUANTITY TO BE DELIVERED                
013300     03 ART-KDKOPTYP         PIC X.                                       
013400*                                 KOD TYP AV INKÖP                        
013500*                                 CODE FOR TYPE OF PURCHASE               
013600     03 FILLER               PIC X(3).                                    
013700     03 ART-KVPROG           PIC S9(7)           COMP-3.                  
013800*                                 ÅRSPROGNOS                              
013900*                                 PROGNOS OF THE YEAR                     
014000     03 ART-KVUPB            PIC S9(5)           COMP-3.                  
014100*                                 ANTAL SOM SKALL UTFALLSPROVAS           
014200*                                 QUANTITY FOR QUALITY CONTROL            
014300     03 ART-PRARTBES         PIC S9(7)V9(2)      COMP-3.                  
014400*                                 BESTÄLLNINGSPRIS I KRONOR               
014500*                                 ORDER PRICE SWEDISH CURRENCY            
014600     03 ART-TEANSINK         PIC X(50).                                   
014700*                                 NOTERING VID FÖRSTA KÖP                 
014800*                                 PROCURER NOTE FIRST PURCHASE            
014900     03 ART-TEARTNOT         PIC X(40).                                   
015000*                                 ARTIKEL NOTERING                        
015100*                                 PART REMARKS NOTE                       
015200     03 ART-TEARTNOT-BASL    PIC X(40).                                   
015300*                                 BASLAGER ARTIKEL NOTERING               
015400*                                 PARTS NOTIFY BASIC STOCK                
015500     03 ART-TEORSAK          PIC X(50).                                   
015600*                                 INFO OM SLAG AV ÅTGÄRD                  
015700*                                 REASON INFORMATION                      
015800     03 ART-TETEKNIK         PIC X(50).                                   
015900*                                 TEKNISK INFO FRÅN PV/LV                 
016000*                                 TECNICAL INFO FROM CARS/TRUCKS          
016100     03 ART-TIANSKREG        PIC S9(7)           COMP-3.                  
016200*                                 REGISTRERINGSDATUM ANSKAFFNING          
016300*                                 REGISTRATION DATE PROCURER              
016400     03 ART-DABASL           PIC 9(8).                                    
016500*                                 BASLAGER MARKNADS KNYTTID               
016600*                                 BASIC STOCK CONNECT TIME                
016700     03 ART-DAFINLEV         PIC 9(8).                                    
016800*                                 PUBLICERINGSDATUM  (AAAAMMDD)           
016900*                                 DATE 1:ST GOODS REC (YYYYMMDD)          
017000     03 ART-TIINKOP          PIC S9(7)           COMP-3.                  
017100*                                 DATUM NÄR INKÖP BEGÄRS                  
017200*                                 DATE FOR PURCHASE REQUEST               
017300     03 ART-TILEVBEG         PIC S9(7)           COMP-3.                  
017400*                                 DATUM NÄR LEVERANS BEGÄRS               
017500*                                 DATE FOR DELIVER REQUEST                
017600     03 ART-TINEDBRY         PIC S9(7)           COMP-3.                  
017700*                                 SLUTTID NEDBRYTNING                     
017800*                                 FINISH-TIME ASSEMBLY PLANNING           
017900     03 ART-TIPLAKOP         PIC S9(7)           COMP-3.                  
018000*                                 PLANERAD KÖPTID                         
018100*                                 PLANNED PURCHASE TIME                   
018200     03 ART-TIREGDAT         PIC S9(7)           COMP-3.                  
018300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
018400*                                 REGISTRATION DATE (YYMMDD)              
018500     03 ART-TIRITB           PIC S9(7)           COMP-3.                  
018600*                                 B-RITNINGSTID                           
018700*                                 B-DRAWING TIME                          
018800     03 ART-TIRITC           PIC S9(7)           COMP-3.                  
018900*                                 C-RITNINGSTID                           
019000*                                 C-DRAWING TIME                          
019100     03 ART-TIRITP           PIC S9(7)           COMP-3.                  
019200*                                 P-RITNINGSTID                           
019300*                                 P-DRAWING TIME                          
019400     03 ART-TISERLEV         OCCURS 5 TIMES                               
019500                             PIC S9(7)           COMP-3.                  
019600*                                 SERIELEVERANS START                     
019700*                                 SERIAL DELIVERY START                   
019800     03 ART-TISLUBER         PIC S9(7)           COMP-3.                  
019900*                                 BEREDNINGS SLUT                         
020000*                                 STOP-TIME PARTS PLANNING                
020100     03 ART-TISTABER         PIC S9(7)           COMP-3.                  
020200*                                 BEREDNINGS START                        
020300*                                 START-TIME PARTS PLANNING               
020400     03 ART-TISTOMREG        PIC S9(7)           COMP-3.                  
020500*                                 STOPPTID MARKNADSREGISTRERING           
020600*                                 STOP-TIME MARKET REGISTRATION           
020700     03 ART-TIUPB            PIC S9(7)           COMP-3.                  
020800*                                 TID NÄR UTFALLSPROV BEGÄRTS             
020900*                                 TIME FOR QUALITY TEST                   
021000     03 ART-TIUPG            PIC S9(7)           COMP-3.                  
021100*                                 TID NÄR UTFALLSPROV GJORTS              
021200*                                 DATE FOR ENDED QUALITY CONTROL          
021300     03 ART-TIUPPDAT         PIC S9(7)           COMP-3.                  
021400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
021500*                                 UPDATING DATE     (YYMMDD)              
021600     03 FILLER               PIC X(9).                                    
021700*** END OF VILMAII-COPY LENGTH= 526 BYTES                                 
