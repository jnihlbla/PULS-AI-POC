000100 01  R32-W407R32A.                                                        
000200*                                 R32                                     
000300*                                 SKAPAS VID ÅTERRAPP AV ARTIKLAR         
000400*                                 PÅ CDC,NÄR LEV.ANM FÅR STATUS 7         
000500*                                 GÄLLER ENBART USA-RETURER               
000600*                                 ORSAKSKOD 54 DISTR 8211 OCH             
000700*                                 ORSAKSKOD 94 DISTR 8111.                
000800     03 R32-KDLEVANM         PIC X.                                       
000900*                                 STATUS LEVERANSANMÄRKNING               
001000     03 R32-SUSTDTOT         PIC S9(11)V9(2)     COMP-3.                  
001100*                                 SUMMA VÄRDE TILL STANDARDPRIS           
001200     03 R32-IDDISTR          PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 R32-IDKUNDNR         PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 R32-IDRAPPNR         PIC 9(7).                                    
001700*                                 RAPPORT NUMMER                          
001800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
