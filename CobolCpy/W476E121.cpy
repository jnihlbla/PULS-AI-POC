000100 01  KLI-W476E121.                                                        
000200*                                 TRANSPORTRELEASEREG.POST                
000300*                                 KOLLIINFO                               
000400     03 KLI-IDPTYP           PIC X(4).                                    
000500*                                 POSTTYP              IDPTYP-004         
000600     03 KLI-IDSHIPM          PIC 9(7).                                    
000700*                                 SKEPPNINGSNUMMER                        
000800     03 KLI-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 KLI-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 KLI-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 KLI-IDKOLLI          PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 KLI-IDORDNR5         PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800     03 KLI-KDFARLIG-KOLLI   PIC S9              COMP-3.                  
001900*                                 KOD FÖR FARLIGT GODS I KOLLI            
002000     03 KLI-KDFRAKT          PIC S9(3)           COMP-3.                  
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200     03 KLI-SUORDV-KOLLI     PIC S9(9)V9(2)      COMP-3.                  
002300*                                 VARUVÄRDE PER KOLLI                     
002400     03 KLI-KDVALISO         PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600     03 KLI-VKORDBTO-KOLLI   PIC S9(6)V9(1)      COMP-3.                  
002700*                                 ORDERVIKT BRUTTO PER KOLLI              
002800     03 KLI-VLORDBTO-KOLLI   PIC S9(4)V9(3)      COMP-3.                  
002900*                                 ORDERVOLYM BRUTTO KOLLI                 
003000     03 KLI-DIKOLLIB         PIC S9(3)           COMP-3.                  
003100*                                 KOLLI-BREDD                             
003200     03 KLI-DIKOLLIH         PIC S9(3)           COMP-3.                  
003300*                                 KOLLI-HÖJD                              
003400     03 KLI-DIKOLLIL         PIC S9(5)           COMP-3.                  
003500*                                 KOLLI-LÄNGD                             
003600     03 KLI-KDORDKL          PIC S9              COMP-3.                  
003700*                                 ORDERKLASS                              
003800     03 KLI-ADGMT.                                                        
003900*                                 GODSMOTTAGARADRESS                      
004000        05 KLI-ADGMT-GATA    PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS GATA                 
004200        05 KLI-ADGMT-PADR    PIC X(35).                                   
004300*                                 GODSMOTTAGARADRESS POSTADRESS           
004400        05 KLI-ADPOST-PNRORT REDEFINES KLI-ADGMT-PADR.                    
004500*                                 POSTNUMMER + ORT                        
004600           07 KLI-ADPOSTNR   PIC X(10).                                   
004700*                                 POSTNUMMER I ADRESS                     
004800           07 KLI-ADCITY     PIC X(25).                                   
004900*                                 BENÄMNING PÅ STAD                       
005000        05 KLI-ADPOST-ORTPNR REDEFINES KLI-ADGMT-PADR.                    
005100*                                 ORT + POSTNUMMER                        
005200           07 KLI-ADCITY     PIC X(25).                                   
005300*                                 BENÄMNING PÅ STAD                       
005400           07 KLI-ADPOSTNR   PIC X(10).                                   
005500*                                 POSTNUMMER I ADRESS                     
005600        05 KLI-ADGMT-LAND    PIC X(35).                                   
005700*                                 GODSMOTTAGARADRESS LAND                 
005800     03 KLI-BEGMT.                                                        
005900*                                 GODSMOTTAGARNAMN                        
006000        05 KLI-BEGMT-RAD1    PIC X(35).                                   
006100*                                 GODSMOTTAGARNAMN RAD 1                  
006200        05 KLI-BEGMT-RAD2    PIC X(35).                                   
006300*                                 GODSMOTTAGARNAMN RAD 2                  
006400*** END OF VILMAII-COPY LENGTH= 231 BYTES                                 
