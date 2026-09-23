000100 01  CREF-WDK629.                                                         
000200*                                 REFILL PARAMETRAR                       
000300*                                 FYSISK NYCKEL = IDDC-REF                
000400     03 CREF-IDDC-REF        PIC X(2).                                    
000500*                                 SÄNDANDE LAGER FÖR REFILL               
000600*                                 SENDING WAREHOUSE FOR REFILL            
000700     03 CREF-FLFLYG          PIC X.                                       
000800*                                 FLYGARTIKEL                             
000900*                                 PART NUMBER SENT BY AIR                 
001000     03 CREF-FLPB-FLYTT      PIC X.                                       
001100*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
001200*                                  REDA PÅ KOPIERING AV PROGNOS           
001300*                                 FLAG                                    
001400     03 CREF-FLREFBEO        PIC X.                                       
001500*                                 AUTOMATISK REFILL BEORDRING?            
001600*                                 AUTOMATIC REFILL ORDERING?              
001700     03 CREF-FLREFILL        PIC X.                                       
001800*                                 REFILLARTIKEL                           
001900*                                 REFILLPART                              
002000     03 CREF-FLREFNYO        PIC X.                                       
002100*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
002200*                                 WAIT FOR NEXT DEMAND                    
002300     03 CREF-FLWILSON        PIC X.                                       
002400*                                 WILSONFORMEL                            
002500*                                 FLAG TO USE WILSON OR NOT               
002600     03 CREF-FLBUYUPD        PIC X.                                       
002700*                                 OM IDPERSONKOD ÄR LÅST                  
002800*                                 IF BUYER UPDATE IS LOCKED               
002900     03 CREF-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
003000*                                 PERSONKOD REFILLANSVARIG                
003100*                                 REFILL RESPONSIBLE ID                   
003200     03 CREF-FLTABUPD        PIC X.                                       
003300*                                 OM REFILLTABELL ÄR LÅST                 
003400*                                 IF REFILLINGTABLE UPDATE LOCKED         
003500     03 CREF-IDREFTAB        PIC X.                                       
003600*                                 IDENTITET REFILLTABELL                  
003700*                                 REFILLINGTABLE IDENTIFIER               
003800     03 CREF-KVPB-PLAN       PIC S9(6)V9(1)      COMP-3.                  
003900*                                 PLANERAT PERIODBEHOV                    
004000*                                 PLANNED PERIOD REQUIREMENTS             
004100     03 CREF-KDREFSTA        PIC X.                                       
004200*                                 STATUS REFILLARTIKEL                    
004300*                                 STATUS REFILLPART                       
004400     03 CREF-KVREFOVL        PIC S9(7)           COMP-3.                  
004500*                                 BERÄKNAD ÖVERLAGERPUNKT                 
004600*                                 CALCULATED OVERSTOCK POINT              
004700     03 CREF-KVREFPKT        PIC S9(7)           COMP-3.                  
004800*                                 BERÄKNAD PÅFYLLNADSPUNKT                
004900*                                 CALCULATED REFILLING POINT              
005000     03 CREF-RESEASON-PLAN   OCCURS 12 TIMES                              
005100                             PIC S9V9(2)         COMP-3.                  
005200*                                 SÄSONGSINDEX INKLUSIVE REFILL           
005300     03 CREF-TIORDREG        PIC S9(7)           COMP-3.                  
005400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005500*                                 ORDER REGISTRATION DATE  YYMMDD         
005600     03 CREF-TIREFEFT        PIC S9(7)           COMP-3.                  
005700*                                 DATUM SENAST EFTERFRÅGAD                
005800*                                 DATE LATEST DEMAND                      
005900     03 CREF-TIREFPAF        PIC S9(7)           COMP-3.                  
006000*                                 DATUM MANUELL PÅFYLLNADSKVANT           
006100*                                 DATE MANUAL REFILLING QTY               
006200     03 CREF-TIREFPKT        PIC S9(7)           COMP-3.                  
006300*                                 DATUM MANUELL REFILLPUNKT               
006400*                                 DATE MANUAL REFILLING POINT             
006500     03 CREF-TIREFSTA        PIC S9(7)           COMP-3.                  
006600*                                 DATUM AKT/PASS REFILLARTIKEL            
006700*                                 DATE ACT/PASS REFILLPART                
006800     03 CREF-TIREFSTO        PIC S9(7)           COMP-3.                  
006900*                                 BEORDRINGSSTOPPAD T.OM.                 
007000*                                 STOPPED FOR ORDERING UNTIL              
007100*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
