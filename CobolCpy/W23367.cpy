000010*** EDIT ALLOWED                                                          
000100 01  W23367.                                                              
000200*                                                                         
000300*                          UPPGIFTER OM EMBLEM TILL CARPAC                
000310*                        ********                                         
000320*                        * VPCA *                                         
000400*                        ********                                         
000500*                                                                         
000600    03  FZ2RECT            PIC  X(04).                                    
000700*                                          RECORD TYPE                    
000800    03  FZ2PART.                                                          
000900      05  IDARTNR          PIC  9(07).                                    
001100      05  FILLER           PIC  X(05).                                    
001101*                                                                         
001110    03  FZ2PART-8          REDEFINES FZ2PART.                             
001120      05  IDARTNR-8        PIC  9(08).                                    
001140      05  FILLER-8         PIC  X(04).                                    
001200*                                          PART NUMBER                    
001210*                                                                         
001220    03  FZ2CAT             PIC  X(10).                                    
001230*                                          CATALOGUE NUMBER               
001240*                                          (BEEMBLEM)                     
001250*                                                                         
001286*                                                                         
009700    03  FZ2FILLER          PIC  X(06).                                    
009800*                                          FILLER                         
009900***END COPY W23367   LENTH=32                                             
