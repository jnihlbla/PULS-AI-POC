000100 01  REQU-WL0171I1.                                                       
000200*                                 REQUEST TO PGM WL0171                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 REQU-KVJUSTKV-IN     PIC 9(7).                                    
000800*                                 JUSTERAD KVANTITET                      
000900     03 REQU-KDAVVTYP        PIC X.                                       
001000*                                 AVVIKELSETYP                            
001100*                                 1=POSITIV.  2=NEGATIV                   
001200     03 REQU-FLANTAL         PIC X.                                       
001300*                                 ANTALJUSTERINGSFLAGGA                   
001400*                                 1 = JA. ANNAT = NEJ.                    
001500*                                 5 = AVV. VID REFILL AV S-LAGER          
001600*                                 5 ANVÄNDS ENDAST I INVENTERING          
001700     03 REQU-FLSLACK         PIC X.                                       
001800*                                 ALLMÄN FLAGGA                           
001900     03 REQU-FLFLYTTN        PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
