000100 01  CREF-W01169X.                                                        
000200*                                 A COPY OF W01169 - TO CREATE WX         
000300*                                 TR FILE IN EDITABLE FORMAT              
000400     03 CREF-IDARTNR         PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 CREF-WDK629.                                                      
000900*                                 REFILL PARAMETRAR                       
001000*                                 FYSISK NYCKEL = IDDC-REF                
001100        05 CREF-IDDC-REF     PIC X(2)                                     
001200                             VALUE SPACES.                                
001300*                                 SÄNDANDE LAGER FÖR REFILL               
001400*                                 SENDING WAREHOUSE FOR REFILL            
001500        05 CREF-FLFLYG       PIC X                                        
001600                             VALUE SPACE.                                 
001700*                                 FLYGARTIKEL                             
001800*                                 PART NUMBER SENT BY AIR                 
001900        05 CREF-FLPB-FLYTT   PIC X                                        
002000                             VALUE SPACE.                                 
002100*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
002200*                                  REDA PÅ KOPIERING AV PROGNOS           
002300*                                 FLAG                                    
002400        05 CREF-FLREFBEO     PIC X                                        
002500                             VALUE SPACE.                                 
002600*                                 AUTOMATISK REFILL BEORDRING?            
002700*                                 AUTOMATIC REFILL ORDERING?              
002800        05 CREF-FLREFILL     PIC X                                        
002900                             VALUE SPACE.                                 
003000*                                 REFILLARTIKEL                           
003100*                                 REFILLPART                              
003200        05 CREF-FLREFNYO     PIC X                                        
003300                             VALUE SPACE.                                 
003400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
003500*                                 WAIT FOR NEXT DEMAND                    
003600        05 CREF-FLWILSON     PIC X                                        
003700                             VALUE SPACE.                                 
003800*                                 WILSONFORMEL                            
003900*                                 FLAG TO USE WILSON OR NOT               
004000        05 CREF-FLBUYUPD     PIC X                                        
004100                             VALUE SPACE.                                 
004200*                                 OM IDPERSONKOD ÄR LÅST                  
004300*                                 IF BUYER UPDATE IS LOCKED               
004400        05 CREF-IDPERSON-BUY PIC Z(2)9                                    
004500                             VALUE ZEROS.                                 
004600*                                 PERSONKOD REFILLANSVARIG                
004700*                                 REFILL RESPONSIBLE ID                   
004800        05 CREF-FLTABUPD     PIC X                                        
004900                             VALUE SPACE.                                 
005000*                                 OM REFILLTABELL ÄR LÅST                 
005100*                                 IF REFILLINGTABLE UPDATE LOCKED         
005200        05 CREF-IDREFTAB     PIC X                                        
005300                             VALUE SPACE.                                 
005400*                                 IDENTITET REFILLTABELL                  
005500*                                 REFILLINGTABLE IDENTIFIER               
005600        05 CREF-KVPB-PLAN    PIC Z(5)9.9                                  
005700                             VALUE ZEROS.                                 
005800*                                 PLANERAT PERIODBEHOV                    
005900*                                 PLANNED PERIOD REQUIREMENTS             
006000        05 CREF-KDREFSTA     PIC X                                        
006100                             VALUE SPACE.                                 
006200*                                 STATUS REFILLARTIKEL                    
006300*                                 STATUS REFILLPART                       
006400        05 CREF-KVREFOVL     PIC Z(6)9                                    
006500                             VALUE ZEROS.                                 
006600*                                 BERÄKNAD ÖVERLAGERPUNKT                 
006700*                                 CALCULATED OVERSTOCK POINT              
006800        05 CREF-KVREFPKT     PIC Z(6)9                                    
006900                             VALUE ZEROS.                                 
007000*                                 BERÄKNAD PÅFYLLNADSPUNKT                
007100*                                 CALCULATED REFILLING POINT              
007200        05 CREF-RESEASON-PLAN                                             
007300                             OCCURS 12 TIMES                              
007400                             PIC 9.9(2)                                   
007500                             VALUE ZEROS.                                 
007600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
007700        05 CREF-TIORDREG     PIC 9(6)                                     
007800                             VALUE ZEROS.                                 
007900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
008000*                                 ORDER REGISTRATION DATE  YYMMDD         
008100        05 CREF-TIREFEFT     PIC 9(6)                                     
008200                             VALUE ZEROS.                                 
008300*                                 DATUM SENAST EFTERFRÅGAD                
008400*                                 DATE LATEST DEMAND                      
008500        05 CREF-TIREFPAF     PIC 9(6)                                     
008600                             VALUE ZEROS.                                 
008700*                                 DATUM MANUELL PÅFYLLNADSKVANT           
008800*                                 DATE MANUAL REFILLING QTY               
008900        05 CREF-TIREFPKT     PIC 9(6)                                     
009000                             VALUE ZEROS.                                 
009100*                                 DATUM MANUELL REFILLPUNKT               
009200*                                 DATE MANUAL REFILLING POINT             
009300        05 CREF-TIREFSTA     PIC 9(6)                                     
009400                             VALUE ZEROS.                                 
009500*                                 DATUM AKT/PASS REFILLARTIKEL            
009600*                                 DATE ACT/PASS REFILLPART                
009700        05 CREF-TIREFSTO     PIC 9(6)                                     
009800                             VALUE ZEROS.                                 
009900*                                 BEORDRINGSSTOPPAD T.OM.                 
010000*                                 STOPPED FOR ORDERING UNTIL              
010100*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
