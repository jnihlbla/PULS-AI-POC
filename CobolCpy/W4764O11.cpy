000100 01  KND-W4764O11.                                                        
000200*                                 TRANSPORTRELEASEREG.POST                
000300*                                 GODSMOTTAGARE                           
000400     03 KND-IDPTYP           PIC X(4).                                    
000500*                                 POSTTYP              IDPTYP-004         
000600     03 KND-IDSHIPM          PIC 9(7).                                    
000700*                                 SKEPPNINGSNUMMER                        
000800     03 KND-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 KND-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 KND-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 KND-IDPRODNR         PIC S9(7)           COMP-3.                  
001500*                                 PRODUKTIONSNUMMER                       
001600     03 KND-IDKOLLI          PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 KND-KVKOLLI          PIC S9(5)           COMP-3.                  
001900*                                 ANTAL KOLLI                             
002000     03 KND-VKORDBTO         PIC S9(6)V9(1)      COMP-3.                  
002100*                                 ORDERVIKT BRUTTO (KG)                   
002200     03 KND-VKARTNTO         PIC S9(4)V9(3)      COMP-3.                  
002300*                                 ARTIKELVIKT NETTO (KG) MED EMB          
002400     03 KND-VLORDBTO         PIC S9(4)V9(3)      COMP-3.                  
002500*                                 ORDERVOLYM BRUTTO (M3)                  
002600     03 KND-BEGMT.                                                        
002700*                                 GODSMOTTAGARNAMN                        
002800        05 KND-BEGMT-RAD1    PIC X(35).                                   
002900*                                 GODSMOTTAGARNAMN RAD 1                  
003000        05 KND-BEGMT-RAD2    PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 2                  
003200     03 KND-ADGMT-GATA       PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS GATA                 
003400     03 KND-ADGMT-PADR       PIC X(35).                                   
003500*                                 GODSMOTTAGARADRESS POSTADRESS           
003600     03 KND-ADGMT-LAND       PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS LAND                 
003800*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
