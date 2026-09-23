000100 01  W3O12701.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O12201                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDDISTR-IN-ATTR      PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 IDDISTR-IN           PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDDISTR-UT           PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 IDARTNR-IN-ATTR      PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 IDARTNR-IN           PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 IDARTNR-UT           PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 IDPTYP-IN-ATTR       PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 IDPTYP-IN            PIC X(3).                                    
002300*                                 POSTTYP                                 
002400     03 IDPTYP-UT            PIC X(3).                                    
002500*                                 POSTTYP                                 
002600     03 OUTPUTLINES          OCCURS 14 TIMES.                             
002700        05 IDPTYP            PIC X(3).                                    
002800*                                 POSTTYP                                 
002900        05 IDKUNDNR          PIC Z(7).                                    
003000*                                 KUNDNUMMER                              
003100        05 IDARTNR           PIC Z(9).                                    
003200*                                 ARTIKELNUMMER                           
003300        05 BEART-ENG         PIC X(25).                                   
003400*                                 ENGELSK ARTIKELBENÄMNING                
003500        05 KVANTAL           PIC Z(7).                                    
003600*                                 ANTAL                                   
003700        05 IDDC              PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900        05 DAREGDAT          PIC Z(8).                                    
004000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004100     03 TEMFSINF             PIC X(55).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300*** END OF VILMAII-COPY LENGTH= 991 BYTES                                 
