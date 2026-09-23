000100 01  W612422.                                                             
000200*                                 COPYTEXT TILL FIL W61242                
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 ADART.                                                            
000800*                                 ARTIKELADRESS I LAGRET                  
000900        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
001000*                                 LAGEROMR≈DE                             
001100        05 ADGANG            PIC S9(3)           COMP-3.                  
001200*                                 G≈NG                                    
001300        05 ADPLATS           PIC S9(5)           COMP-3.                  
001400*                                 LAGERPLATSNUMMER                        
001500     03 BEART                PIC X(25).                                   
001600*                                 ARTIKELBENƒMNING                        
001700     03 DAREGDAT             PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001900     03 FLKDFARLIG           PIC X.                                       
002000     03 FLBACKORDER          PIC X.                                       
002100     03 FLPRIO               PIC X.                                       
002200     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
002300*                                 PERSONKOD REFILLANSVARIG                
002400     03 KDARTURS             PIC X(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD                     
002600     03 KVAVIS               PIC S9(7)           COMP-3.                  
002700*                                 AVISERAT ANTAL                          
002800     03 VKART                PIC S9(7)           COMP-3.                  
002900*                                 ARTIKELVIKT (G)                         
003000     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003100*                                 ARTIKELVOLYM NETTO (CM3)                
003200     03 KVAINF.                                                           
003300        05 TEKVAINF-EXT      OCCURS 7 TIMES                               
003400                             PIC X(79).                                   
003500*                                 KVALITETS INFORMATION EXTERNT           
003600*** END OF VILMAII-COPY LENGTH= 621 BYTES                                 
