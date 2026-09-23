000100 01  MICO-W402MICO.                                                       
000200*                                 LAYOUT FÖR ORDER(ORDER/DC-INFO)         
000300*                                 SOM SKICKAS TILL FLS/MIC                
000400*                                 ANVÄNDS I W40293 SOM SKICKAR            
000500*                                 ORDER TILL FLS/MIC VIA MQ               
000600     03 MICO-IDPTYP          PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 MICO-TIMESTAMP       PIC X(16).                                   
000900     03 MICO-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MICO-IDSYSTEM        PIC X(4).                                    
001200*                                 VOLVO VCCS SYSTEMNUMMER                 
001300     03 MICO-IDLAND-FOM      PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500     03 MICO-IDLAND-TOM      PIC X(2).                                    
001600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001700     03 MICO-IDORDNR7        PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MICO-TIRFSDAT        PIC 9(6).                                    
002000*                                 KLART FÖR TRANSPORT ÅÅMMDD              
002100     03 MICO-IDPARTNR        PIC X(9).                                    
002200*                                 FINANCIELL KUND                         
002300     03 MICO-IDDISTR         PIC 9(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MICO-IDKUNDNR        PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 MICO-IDPARTNER       PIC X(9).                                    
002800*                                 PARTNER ID                              
002900     03 MICO-BEGMT-1-1       PIC X(35).                                   
003000*                                 GODSMOTTAGARNAMN RAD 1                  
003100     03 MICO-BEGMT-1-2       PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 2                  
003300     03 MICO-ADGMT-GATA-1    PIC X(35).                                   
003400*                                 GODSMOTTAGARADRESS GATA                 
003500     03 MICO-ADPOSTNR        PIC X(10).                                   
003600*                                 POSTNUMMER I ADRESS                     
003700     03 MICO-ADCITY          PIC X(25).                                   
003800*                                 BENÄMNING PÅ STAD                       
003900     03 MICO-BEGMT-2-1       PIC X(35).                                   
004000*                                 GODSMOTTAGARNAMN RAD 1                  
004100     03 MICO-BEGMT-2-2       PIC X(35).                                   
004200*                                 GODSMOTTAGARNAMN RAD 2                  
004300     03 MICO-ADGMT-GATA-2    PIC X(35).                                   
004400*                                 GODSMOTTAGARADRESS GATA                 
004500     03 MICO-ADGMT-PADR-2    PIC X(35).                                   
004600*                                 GODSMOTTAGARADRESS POSTADRESS           
004700*** END OF VILMAII-COPY LENGTH= 350 BYTES                                 
