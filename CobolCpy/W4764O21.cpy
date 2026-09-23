000100 01  KLI-W4764O21.                                                        
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
001400     03 KLI-IDPRODNR         PIC S9(7)           COMP-3.                  
001500*                                 PRODUKTIONSNUMMER                       
001600     03 KLI-IDKOLLI          PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 KLI-IDORDNR5         PIC 9(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 KLI-BEKUNDRF         PIC X(15).                                   
002100*                                 KUNDENS REFERENS                        
002200     03 KLI-KDFARLIG-KOLLI   PIC S9              COMP-3.                  
002300*                                 KOD FÖR FARLIGT GODS I KOLLI            
002400     03 KLI-KDFRAKT          PIC S9(3)           COMP-3.                  
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 KLI-SUORDV-KOLLI     PIC S9(9)V9(2)      COMP-3.                  
002700*                                 VARUVÄRDE PER KOLLI                     
002800     03 KLI-KDVALISO         PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000     03 KLI-VKORDBTO-KOLLI   PIC S9(6)V9(1)      COMP-3.                  
003100*                                 ORDERVIKT BRUTTO PER KOLLI              
003200     03 KLI-VLORDBTO-KOLLI   PIC S9(4)V9(3)      COMP-3.                  
003300*                                 ORDERVOLYM BRUTTO KOLLI                 
003400     03 KLI-DIKOLLIB         PIC S9(3)           COMP-3.                  
003500*                                 KOLLI-BREDD                             
003600     03 KLI-DIKOLLIH         PIC S9(3)           COMP-3.                  
003700*                                 KOLLI-HÖJD                              
003800     03 KLI-DIKOLLIL         PIC S9(5)           COMP-3.                  
003900*                                 KOLLI-LÄNGD                             
004000     03 KLI-KDORDKL          PIC S9              COMP-3.                  
004100*                                 ORDERKLASS                              
004200     03 KLI-ADGMT.                                                        
004300*                                 GODSMOTTAGARADRESS                      
004400        05 KLI-ADGMT-GATA    PIC X(35).                                   
004500*                                 GODSMOTTAGARADRESS GATA                 
004600        05 KLI-ADGMT-PADR    PIC X(35).                                   
004700*                                 GODSMOTTAGARADRESS POSTADRESS           
004800        05 KLI-ADPOST-PNRORT REDEFINES KLI-ADGMT-PADR.                    
004900*                                 POSTNUMMER + ORT                        
005000           07 KLI-ADPOSTNR   PIC X(10).                                   
005100*                                 POSTNUMMER I ADRESS                     
005200           07 KLI-ADCITY     PIC X(25).                                   
005300*                                 BENÄMNING PÅ STAD                       
005400        05 KLI-ADPOST-ORTPNR REDEFINES KLI-ADGMT-PADR.                    
005500*                                 ORT + POSTNUMMER                        
005600           07 KLI-ADCITY     PIC X(25).                                   
005700*                                 BENÄMNING PÅ STAD                       
005800           07 KLI-ADPOSTNR   PIC X(10).                                   
005900*                                 POSTNUMMER I ADRESS                     
006000        05 KLI-ADGMT-LAND    PIC X(35).                                   
006100*                                 GODSMOTTAGARADRESS LAND                 
006200     03 KLI-BEGMT.                                                        
006300*                                 GODSMOTTAGARNAMN                        
006400        05 KLI-BEGMT-RAD1    PIC X(35).                                   
006500*                                 GODSMOTTAGARNAMN RAD 1                  
006600        05 KLI-BEGMT-RAD2    PIC X(35).                                   
006700*                                 GODSMOTTAGARNAMN RAD 2                  
006800*** END OF VILMAII-COPY LENGTH= 250 BYTES                                 
