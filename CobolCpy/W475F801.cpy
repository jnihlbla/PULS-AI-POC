000100 01  W475F801-CTX.                                                        
000200*                                 COPYTEXT FÖR ATT SKAPA VR-KOPPL         
000300*                                 POSTTYP VR1                             
000400     03 KDFRAKT              PIC S9(3)           COMP-3.                  
000500*                                 FRAKTSÄTT DC TILL KUND                  
000600     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
000700*                                 EMBALLAGE O HANTERINGSKOST              
000800     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
000900*                                 FRAKTKOSTNAD                            
001000     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
001100*                                 FÖRSÄKRINGSPREMIE                       
001200     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
001300*                                 SUMMA FAKTURERAT BELOPP                 
001400     03 TIFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001600     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
001700*                                 VALUTAKURS                              
001800     03 KDVALISO             PIC X(3).                                    
001900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002000     03 KDREFNOT             PIC X(2).                                    
002100*                                 FAKTURA NOTERINGAR                      
002200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
