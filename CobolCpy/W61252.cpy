000100 01  SHIST-W61252.                                                        
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 UPPFÖLJNING, VECKOVIS                   
000400*                                 ALLA R32                                
000500     03 SHIST-IDINLEV        PIC S9(15)          COMP-3.                  
000600*                                 INLEVERANS NUMMER                       
000700     03 SHIST-KDFRAKT        PIC S9(3)           COMP-3.                  
000800*                                 FRAKTSÄTT DC TILL KUND                  
000900     03 SHIST-IDDC           PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 SHIST-TIINLMOT       PIC S9(7)           COMP-3.                  
001200*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
001300     03 SHIST-TIINLMTI       PIC 9(4).                                    
001400*                                 MOTTAGNINGSTID     (TTMM)               
001500     03 SHIST-TIINLINL       PIC S9(7)           COMP-3.                  
001600*                                 RAPPORTERINGSDATUM INLAGD (R32)         
001700     03 SHIST-TIINLITI       PIC 9(4).                                    
001800*                                 RAPPORTERINGSTID   INLAGD (R32)         
001900*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
