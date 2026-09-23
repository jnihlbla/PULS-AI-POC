000100 01  MOD-W2O14901.                                                        
000200*                                 COPYTEXT FÖR MOD W2014900               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-SHIP-IN  PIC X(5).                                    
000800*                                 SKEPPANDE LEVERANTÖR                    
000900     03 MOD-IDLEVNR-SHIP-UT  PIC X(5).                                    
001000*                                 SKEPPANDE LEVERANTÖR                    
001100     03 MOD-AREA             OCCURS 10 TIMES.                             
001200        05 MOD-KDCMD-UPD-ATTR                                             
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-KDCMD-UPD     PIC X.                                       
001600*                                 RAD-UPPDATERINGSKOMMANDO                
001700*                                  BLANK  = INGENTING                     
001800*                                  D , B  = DELETE                        
001900*                                  R , Ä  = REPLACE                       
002000*                                  I,N,A  = INSERT                        
002100*                                  S , V  = SELECT                        
002200*                                  P , P  = PRINT                         
002300*                                  C , K  = COPY                          
002400        05 MOD-IDLEVNR-SHIP  PIC X(5).                                    
002500*                                 SKEPPANDE LEVERANTÖR                    
002600        05 MOD-IDANSK-FOM    PIC Z(2)9.                                   
002700*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
002800        05 MOD-IDANSK-TOM    PIC Z(2)9.                                   
002900*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
003000        05 MOD-DAAVROP-FOM   PIC 9(4).                                    
003100*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003200*                                 (ÅÅVV)                                  
003300        05 MOD-DAAVROP-TOM   PIC 9(4).                                    
003400*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003500*                                 (ÅÅVV)                                  
003600        05 MOD-DAAVROP-TFOM  PIC 9(4).                                    
003700*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003800*                                 (ÅÅVV)                                  
003900     03 MOD-IDLEVNR-SHIP-UPD-ATTR                                         
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDLEVNR-SHIP-UPD PIC X(5).                                    
004300*                                 SKEPPANDE LEVERANTÖR                    
004400     03 MOD-IDANSK-FOM-UPD-ATTR                                           
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDANSK-FOM-UPD   PIC Z(2)9.                                   
004800*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
004900     03 MOD-IDANSK-TOM-UPD-ATTR                                           
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDANSK-TOM-UPD   PIC Z(2)9.                                   
005300*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
005400     03 MOD-DAAVROP-FOM-UPD-ATTR                                          
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-DAAVROP-FOM-UPD  PIC 9(4).                                    
005800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005900*                                 (ÅÅVV)                                  
006000     03 MOD-DAAVROP-TOM-UPD-ATTR                                          
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-DAAVROP-TOM-UPD  PIC 9(4).                                    
006400*                                 AVSÄNDNINGSVECKA (PLANERAD)             
006500*                                 (ÅÅVV)                                  
006600     03 MOD-DAAVROP-TFOM-UPD-ATTR                                         
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-DAAVROP-TFOM-UPD PIC 9(4).                                    
007000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
007100*                                 (ÅÅVV)                                  
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 404 BYTES                                 
