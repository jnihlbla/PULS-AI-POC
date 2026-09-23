000100 01  MID-W4I30101.                                                        
000200*                                 MID-COPYTEXT FÖR W40301                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-RAD              OCCURS 13 TIMES.                             
000800        05 MID-IDDISTR       PIC 9(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000        05 MID-IDPRODNR      PIC 9(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200        05 MID-FLAVVPACK     PIC X.                                       
001300*                                 AVVIKELSE VID PACKNINGSRAPP?            
001400        05 MID-KDKOLLI       PIC X(8).                                    
001500*                                 KOLLIKOD                                
001600        05 MID-VKORDBTO      PIC 9(7).                                    
001700*                                 ORDERVIKT BRUTTO (KG)                   
001800        05 MID-KDEMBTYP      PIC 9.                                       
001900*                                 EMBALLAGETYP                            
002000        05 MID-IDPRODNR-SAMP PIC 9(7).                                    
002100*                                 PRODUKTIONSNUMMER SAMPACKNING           
