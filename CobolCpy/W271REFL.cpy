000100 01  REFL-W271REFL.                                                       
000200*                                 LÄNKAREA TILL W271REFL.                 
000300*                                                                         
000400*                                 INSTRUKTION:                            
000500*                                                                         
000600*                                 FYLL I INDATA:                          
000700*                                 LAGER        (OBLIGATORISKT)            
000800*                                 STANDARDPRIS (NOLL/IFYLLT)              
000900*                                 OM MANUELL PÅFYLLNADSPUNKT ÄR           
001000*                                 SATT FYLL I IN-KVREFPKT                 
001100*                                 OM MANUELL PÅFYLLNADSKVANT ÄR           
001200*                                 SATT FYLL I IN-KVREFBER                 
001300*                                 LAGEROMRÅDE (NOLL/IFYLLT)               
001400*                                                                         
001500*                                 ANROPA W271REFL USING                   
001600*                                        REFL-W271REFL                    
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
003200           07 REFL-FLWILSON  PIC X.                                       
003300*                                 WILSONFORMEL                            
003400           07 REFL-PRARTBES  PIC S9(7)V9(2)      COMP-3.                  
003500*                                 BESTÄLLNINGSPRIS I KRONOR               
003600           07 REFL-IN-LEADTID-BEHOV                                       
003700                             PIC S9(7)V9(2).                              
003800           07 REFL-IN-KVREFPKT                                            
003900                             PIC S9(7)           COMP-3.                  
004000*                                 BERÄKNAD PÅFYLLNADSPUNKT                
004100           07 REFL-IN-KVREFBER                                            
004200                             PIC S9(7)           COMP-3.                  
004300*                                 BERÄKNAD REFILLINGKVANTITET             
004400           07 REFL-IN-IDLEVNR-DC                                          
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700           07 REFL-NDC-KVDAGAR-TBT-DC                                     
004800                             PIC S9(3)           COMP-3.                  
004900*                                 TOT ANTAL DAGAR HEMTAGNINGSTID          
005000           07 REFL-RESEASON  OCCURS 12 TIMES                              
005100                             PIC S9V9(2)         COMP-3.                  
005200*                                 SÄSONGSINDEX                            
005300           07 REFL-FLFLYG    PIC X.                                       
005400*                                 FLYGARTIKEL                             
005500           07 REFL-BINNDAY-TIAAMMDD                                       
005600                             PIC 9(6).                                    
005700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005800           07 REFL-IDARTNR   PIC S9(9)           COMP-3.                  
005900*                                 ARTIKELNUMMER                           
006000           07 REFL-IN-FLSIM  PIC X.                                       
006100           07 REFL-IN-KVPB-REF                                            
006200                             PIC S9(6)V9(1)      COMP-3.                  
006300*                                 PERIODBEHOV REFILLING                   
006400           07 REFL-IN-KVPBREOI                                            
006500                             PIC S9(6)V9(1)      COMP-3.                  
006600*                                 PERIODBEHOV FÖR REFILL OI               
006700        05 REFL-UTDATA.                                                   
006800           07 REFL-KVREFPKT  PIC S9(7)           COMP-3.                  
006900*                                 BERÄKNAD PÅFYLLNADSPUNKT                
007000           07 REFL-KVREFBER  PIC S9(7)           COMP-3.                  
007100*                                 BERÄKNAD REFILLINGKVANTITET             
007200           07 REFL-KVREFOVL  PIC S9(7)           COMP-3.                  
007300*                                 BERÄKNAD ÖVERLAGERPUNKT                 
007400           07 REFL-KLASS     PIC X(3).                                    
007500        05 REFL-KDSVAR       PIC X.                                       
007600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007700*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
