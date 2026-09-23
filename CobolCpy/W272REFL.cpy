000100 01  REFL-W272REFL.                                                       
000200*                                 LÄNKAREA TILL W272REFL.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 LAGER        (OBLIGATORISKT)            
000800*                                 STANDARDPRIS (NOLL/IFYLLT)              
000900*                                 PERIODBEHOV  (NOLL/IFYLLT)              
001000*                                 OM MANUELL PÅFYLLNADSPUNKT ÄR           
001100*                                 SATT FYLL I IN-KVREFPKT                 
001200*                                 OM MANUELL PÅFYLLNADSKVANT ÄR           
001300*                                 SATT FYLL I IN-KVREFBER                 
001400*                                                                         
001500*                                 ANROPA W272REFL USING                   
001600*                                        REFL-W272REFL                    
001700*                                                                         
001800*                                 OM LAGRET OK (KDSVAR=SPACE),            
001900*                                 FINNS I SVARET UPPGIFTER OM             
002000*                                 LAGRET SAMT REFILL-INFO FÖR             
002100*                                 ANGIVEN KLASS AV ARTIKEL                
002200*                                 (STANDARDPRIS/PERIODBEHOV).             
002300*                                                                         
002400     03 REFL-IN-UTDATA.                                                   
002500        05 REFL-INDATA.                                                   
002600           07 REFL-IDDC      PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800           07 REFL-IDDC-REF  PIC X(2).                                    
002900*                                 SÄNDANDE LAGER FÖR REFILL               
003000           07 REFL-IDREFTAB  PIC X.                                       
003100*                                 IDENTITET REFILLTABELL                  
003200           07 REFL-FLREFBEO  PIC X.                                       
003300*                                 AUTOMATISK REFILL BEORDRING?            
003400           07 REFL-FLWILSON  PIC X.                                       
003500*                                 WILSONFORMEL                            
003600           07 REFL-PRARTBES  PIC S9(7)V9(2)      COMP-3.                  
003700*                                 BESTÄLLNINGSPRIS I KRONOR               
003800           07 REFL-IN-LEADTID-BEHOV                                       
003900                             PIC S9(7)V9(2).                              
004000           07 REFL-IN-KVREFPKT                                            
004100                             PIC S9(7)           COMP-3.                  
004200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
004300           07 REFL-IN-KVREFBER                                            
004400                             PIC S9(7)           COMP-3.                  
004500*                                 BERÄKNAD REFILLINGKVANTITET             
004600           07 REFL-FLFLYG    PIC X.                                       
004700*                                 FLYGARTIKEL                             
004800           07 REFL-BINNDAY-TIAAMMDD                                       
004900                             PIC 9(6).                                    
005000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005100           07 REFL-IDARTNR   PIC S9(9)           COMP-3.                  
005200*                                 ARTIKELNUMMER                           
005300        05 REFL-UTDATA.                                                   
005400           07 REFL-KVREFPKT  PIC S9(7)           COMP-3.                  
005500*                                 BERÄKNAD PÅFYLLNADSPUNKT                
005600           07 REFL-KVREFBER  PIC S9(7)           COMP-3.                  
005700*                                 BERÄKNAD REFILLINGKVANTITET             
005800           07 REFL-KVREFOVL  PIC S9(7)           COMP-3.                  
005900*                                 BERÄKNAD ÖVERLAGERPUNKT                 
006000           07 REFL-KLASS     PIC X(3).                                    
006100           07 REFL-KVSLAGER  PIC S9(7)           COMP-3.                  
006200*                                 SÄKERHETSLAGER                          
006300        05 REFL-KDSVAR       PIC X.                                       
006400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006500        05 REFL-FILLER       PIC X(30).                                   
006600*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  
