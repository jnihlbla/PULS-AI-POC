000100 01  MID-W4I34501.                                                        
000200*                                 MID-COPYTEXT FÖR W40345                 
000300     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000400*                                 TRANSPORTIDENTITET                      
000500     03 MID-IDVO-IN          PIC 9(2).                                    
000600*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-KDFRAKT-SKTRP-IN PIC X(2).                                    
001000*                                 FRAKTSÄTT DC TILL KUND                  
001100     03 MID-IDKOLLI-SAMP-IN  PIC X(5).                                    
001200*                                 SAMPACKNINGSKOLLINUMMER                 
001300     03 MID-IDTRPTNR-UT      PIC X(3).                                    
001400*                                 TRANSPORTIDENTITET                      
001500     03 MID-IDVO-UT          PIC 9(2).                                    
001600*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-KDFRAKT-SKTRP-UT PIC X(2).                                    
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 MID-IDKOLLI-SAMP-UT  PIC X(5).                                    
002200*                                 SAMPACKNINGSKOLLINUMMER                 
002300     03 MID-KEYFIELD.                                                     
002400*                                 NYCKELFÄLT                              
002500        05 MID-IDKOLLI-SAMP  PIC 9(5).                                    
002600*                                 SAMPACKNINGSKOLLINUMMER                 
002700     03 MID-INPUT.                                                        
002800*                                 INMATNINGSFÄLT                          
002900        05 MID-KDATGSKLI-UP  PIC X.                                       
003000*                                 TYP AV ÅTGÄRD FÖR SAMLINGSKOLLI         
003100        05 MID-KDKOLLI-SAMP  PIC X(8).                                    
003200*                                 KOD BÄRKOLLI FÖR SAMPACKNING            
003300        05 MID-DIKOLLIH-SAMP PIC 9(3).                                    
003400*                                 KOLLI-HÖJD                              
003500        05 MID-KDPRTVAL-ADR  PIC X(2).                                    
003600*                                 PRINTER-VAL KOD ADRESS FLAGGA           
003700        05 MID-KDPRTVAL-FS   PIC X(2).                                    
003800*                                 PRINTER-VAL KOD FÖLJESEDEL              
003900        05 MID-FLPRT-ADR     PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100        05 MID-FLPRT-FS      PIC X.                                       
004200*                                 ALLMÄN FLAGGA                           
004300        05 MID-FLKLAR        PIC X.                                       
004400*                                 AVSLUTNINGSMARKERING                    
004500        05 MID-IDDISTR       PIC 9(4).                                    
004600*                                 DISTRIKTNUMMER                          
004700        05 MID-IDKUNDNR      PIC 9(6).                                    
004800*                                 KUNDNUMMER                              
004900        05 MID-IDORDNR7      PIC 9(7).                                    
005000*                                 ORDERNUMMER                             
005100        05 MID-IDKOLLI       PIC 9(5).                                    
005200*                                 KOLLINUMMER                             
005300*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
