000100 01  MOD-W1O54201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 ARTIKELFRÅGA MASTER                     
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-KDFORDON-IN-ATTR PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-KDFORDON-IN      PIC X(2).                                    
001500*                                 FORDONSSLAG                             
001600     03 MOD-KDFORDON-UT      PIC X(2).                                    
001700*                                 FORDONSSLAG                             
001800     03 MOD-IDKATNR-IN-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-IDKATNR-IN       PIC X(5).                                    
002100*                                 KATALOG-ID                              
002200     03 MOD-IDKATNR-UT       PIC X(5).                                    
002300*                                 KATALOG-ID                              
002400     03 MOD-SEGM-NYCKEL.                                                  
002500*                                 DOLD SEGMENT NYCKEL                     
002600        05 MOD-IDFORDON      PIC 9(3).                                    
002700*                                 FORDONSSLAG                             
002800        05 MOD-TIOMBRYT-9KOMPL                                            
002900                             PIC 9(7).                                    
003000*                                 OMBRYTNINGSDATUM 9-KOMPLEMENT           
003100     03 MOD-BEART            PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300     03 MOD-KDMASTAT         PIC X(2).                                    
003400*                                 ARTIKELSTATUS PÅ MASTERREGISTER         
003500     03 MOD-LINE             OCCURS 13 TIMES.                             
003600*                                 RAD                                     
003700        05 MOD-COL           OCCURS 3 TIMES.                              
003800*                                 KOLUMN                                  
003900           07 MOD-KDCMD      PIC X.                                       
004000*                                 RAD-UPPDATERINGSKOMMANDO                
004100*                                  BLANK  = INGENTING                     
004200*                                  D , B  = DELETE                        
004300*                                  R , Ä  = REPLACE                       
004400*                                  I , N  = INSERT                        
004500*                                  S , V  = SELECT                        
004600           07 MOD-BEMASTER   PIC X(12).                                   
004700*                                 MASTERNAMN FÖR FORDON                   
004800           07 MOD-IDKATNR    PIC Z(4)9.                                   
004900*                                 KATALOG-ID                              
005000     03 MOD-TEMFSINF         PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*** END OF VILMAII-COPY LENGTH= 874 BYTES                                 
