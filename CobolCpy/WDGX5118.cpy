000100 01  5118-WDGX5118.                                                       
000200*                                 EKONOMI                                 
000300*                                 VALUTAKODSREGISTER                      
000400*                                 ENLIGT ISO-STANDARD                     
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (KDVALISO + LOW-VALUE)                  
000700     03 5118-KDVALISO        PIC X(3).                                    
000800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000900*                                 CURRENCY CODE BY ISO-STANDARD.          
001000     03 5118-LOW-VALUE       PIC X(2).                                    
001100     03 5118-PRKURS          PIC S9(5)V9(2)      COMP-3.                  
001200*                                 VALUTAKURS           PRKURS-002         
001300     03 5118-REVALUTA        PIC S9(3)           COMP-3.                  
001400*                                 OMRÄKNINGSTAL FÖR VALUTA                
001500*                                 CONVERT VALUE FOR CURRENCY CODE         
001600     03 FILLER               PIC X(9).                                    
001700*** END COPY WDGX5118C0  LENGTH=20                                        
