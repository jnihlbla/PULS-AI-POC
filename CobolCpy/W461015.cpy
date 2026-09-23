000100 01  BYPASS-W461015.                                                      
000200*                                 ORDER TRANS OKÄND I VIPS                
000300*                                 TILL NOAC PT 015                        
000400     03 BYPASS-IDPTYP        PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 BYPASS-IDDISTR       PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 BYPASS-IDKUNDNR      PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 BYPASS-IDORDNR       PIC S9(7)           COMP-3.                  
001100*                                 ORDERNR             IDORDNR-002         
001200     03 BYPASS-KDORDKL       PIC S9              COMP-3.                  
001300*                                 ORDERKLASS                              
001400     03 BYPASS-IDARTNR       PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600     03 BYPASS-REKSIFFR      PIC S9              COMP-3.                  
001700*                                 KONTROLLSIFFRA                          
001800     03 BYPASS-BERADREF      PIC X(10).                                   
001900*                                 KUNDENS RADREFERENS                     
002000     03 BYPASS-BEVOLREF      PIC X(10).                                   
002100*                                 VOLVO REFERENS                          
002200     03 BYPASS-KVBEART       PIC S9(7)           COMP-3.                  
002300*                                 BESTÄLLT ANTAL ARTIKLAR                 
002400     03 BYPASS-KDFAKTYP      PIC X.                                       
002500*                                 FAKTURATYP                              
002600     03 BYPASS-KDDSP         PIC S9              COMP-3.                  
002700*                                 PÅVERKAN PÅ DSP                         
002800     03 BYPASS-FLABON        PIC X.                                       
002900*                                 ABBONEMANGSDLAGGA                       
003000     03 BYPASS-KDTPOTYP      PIC S9              COMP-3.                  
003100*                                 TYP AV TIDPLANERAD ORDER                
003200     03 FILLER               PIC X(4).                                    
003300*** END COPY W461015     LENGTH=53                                        
