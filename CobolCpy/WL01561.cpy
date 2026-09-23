000100 01  DOC-WL01561.                                                         
000200*                                 PRINT QUERY AT LDC,PGM WL0156           
000300     03 DOC-IDAFPRCD         PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 DOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 DOC-IDDISTR          PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 DOC-IDKUNDNR         PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 DOC-IDRAPPNR         PIC Z(6)9.                                   
001200*                                 RAPPORT NUMMER                          
001300     03 DOC-KVKOLLI          PIC Z(3)9.                                   
001400*                                 ANTAL KOLLI                             
001500     03 DOC-TERETNOT         PIC X(20).                                   
001600*                                 FRI NOTERING RETURER                    
001700     03 DOC-IDDC-RET         PIC X(2).                                    
001800*                                 MOTTAGANDE LAGER FÖR RETURER            
001900*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
