000100 01  MOD-W4O34501.                                                        
000200*                                 MODCOPYTEXT TILL W40345.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRPTNR-IN      PIC Z(2)9.                                   
000800*                                 TRANSPORTIDENTITET                      
000900     03 MOD-IDVO-IN          PIC 9(2).                                    
001000*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-KDFRAKT-SKTRP-IN PIC Z9.                                      
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 MOD-IDKOLLI-SAMP-IN  PIC X(5).                                    
001600*                                 SAMPACKNINGSKOLLINUMMER                 
001700     03 MOD-IDTRPTNR-UT      PIC Z(2)9.                                   
001800*                                 TRANSPORTIDENTITET                      
001900     03 MOD-IDVO-UT          PIC 9(2).                                    
002000*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-KDFRAKT-SKTRP-UT PIC Z9.                                      
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500     03 MOD-IDKOLLI-SAMP-UT  PIC X(5).                                    
002600*                                 SAMPACKNINGSKOLLINUMMER                 
002700     03 MOD-KEYFIELD.                                                     
002800*                                                                         
002900        05 MOD-IDKOLLI-SAMP  PIC Z(4)9.                                   
003000*                                 SAMPACKNINGSKOLLINUMMER                 
003100     03 MOD-KDSTASKLI        PIC X.                                       
003200*                                 STATUS SAMLINGSKOLLI                    
003300     03 MOD-KDATGSKLI        PIC X.                                       
003400*                                 TYP AV ÅTGÄRD FÖR SAMLINGSKOLLI         
003500     03 MOD-UPD.                                                          
003600*                                                                         
003700        05 MOD-KDATGSKLI-UP-ATTR                                          
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-KDATGSKLI-UP  PIC X.                                       
004100*                                 TYP AV ÅTGÄRD FÖR SAMLINGSKOLLI         
004200        05 MOD-KDKOLLI-SAMP-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDKOLLI-SAMP  PIC X(8).                                    
004600*                                 KOD BÄRKOLLI FÖR SAMPACKNING            
004700        05 MOD-DIKOLLIH-SAMP-ATTR                                         
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-DIKOLLIH-SAMP PIC Z(2)9.                                   
005100*                                 KOLLI-HÖJD                              
005200        05 MOD-KDPRTVAL-ADR-ATTR                                          
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KDPRTVAL-ADR  PIC X(2).                                    
005600*                                 PRINTER-VAL KOD ADRESS FLAGGA           
005700        05 MOD-KDPRTVAL-FS-ATTR                                           
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-KDPRTVAL-FS   PIC X(2).                                    
006100*                                 PRINTER-VAL KOD FÖLJESEDEL              
006200        05 MOD-FLPRT-ADR-ATTR                                             
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-FLPRT-ADR     PIC X.                                       
006600*                                 ALLMÄN FLAGGA                           
006700        05 MOD-FLPRT-FS-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-FLPRT-FS      PIC X.                                       
007000*                                 ALLMÄN FLAGGA                           
007100        05 MOD-FLKLAR-ATTR   PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-FLKLAR        PIC X.                                       
007400*                                 AVSLUTNINGSMARKERING                    
007500        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-IDDISTR       PIC 9(4).                                    
007800*                                 DISTRIKTNUMMER                          
007900        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-IDKUNDNR      PIC 9(6).                                    
008200*                                 KUNDNUMMER                              
008300        05 MOD-IDORDNR7-ATTR PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-IDORDNR7      PIC 9(7).                                    
008600*                                 ORDERNUMMER                             
008700        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-IDKOLLI       PIC 9(5).                                    
009000*                                 KOLLINUMMER                             
009100     03 MOD-TEMFSINF         PIC X(55).                                   
009200*                                 INFORMATIONSMEDDELANDE                  
009300*** END OF VILMAII-COPY LENGTH= 199 BYTES                                 
