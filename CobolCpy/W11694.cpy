000100 01  W11694.                                                              
000200*                                 PARTSINFO FRÅN PRICE SC.                
000300*                                 FÖR DOM FLESTA FÄLTEN PÅ                
000400*                                 PÅ FILEN HAR VI REDAN INFON             
000500*                                 LAGRAD I PULS.                          
000600*                                 DÄRFÖR ÄR DOM FLESTA FÄLTEN             
000700*                                 DEFINIERADE MED FILLER.                 
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 FILLER               PIC X(4).                                    
001100     03 IDARTNR17            PIC X(17).                                   
001200*                                 17-STÄLLIGT ARTIKELNUMMER FÖR           
001300*                                 AS400 (PRICE SC).                       
001400*                                 FORMATET ÄR VÄNSTERJUSTERAT             
001500*                                 MED BLANKTECKEN SOM UTFYLLNAD.          
001600     03 PRARTBTO-SC          PIC 9(9)V9(2).                               
001700*                                 BRUTTOPRIS PER SÄLJBOLAG                
001800     03 FILLER               PIC X(150).                                  
001900     03 IDLEVNR              PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 FILLER               PIC X(20).                                   
002200*** END OF VILMAII-COPY LENGTH= 210 BYTES                                 
