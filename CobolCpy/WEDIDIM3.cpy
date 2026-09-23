000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI DIM MEASURMENT                                                   
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO SPECIFY MEASURMENT.                                              
000036*                                                                         
000040*                                                                         
000100 01  WEDIDIM3.                                                            
000230     03 DIM3-IDPTYP                            PIC X(03).                 
000240*                                              DIM                        
000501     03 DIM3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 051               
000503*                                                                         
000515     03 DIM3-6145-DIMENSION-QUAL               PIC X(03).                 
000516*                                                                         
000517     03 DIM3-C211-DIMENSIONS.                                             
000518*                                                                         
000519        05 DIM3-6411-MEASURE-UNIT-QUAL         PIC X(03).                 
000520*                                                                         
000521        05 DIM3-6168-LENGTH-DIMENSION          PIC X(15).                 
000522*                                                                         
000523        05 DIM3-6140-WIDTH-DIMENSION           PIC X(15).                 
000530*                                                                         
000540        05 DIM3-6008-HEIGHT-DIMENSION          PIC X(15).                 
000563*                                                                         
000570*** END OF VILMAII-COPY LENGTH=57                                         
