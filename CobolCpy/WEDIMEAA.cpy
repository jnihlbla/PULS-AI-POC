000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI MEA GOODS ITEM DETAILS                                           
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO SPECIFY PSYSICAL MEASUREMENS, INCLUDING DIMENSIONS               
000035*     AND WEIGHTS                                                         
000036*                                                                         
000040*                                                                         
000100 01  WEDIMEA.                                                             
000230     03 MEA-IDPTYP                             PIC X(03).                 
000240*                                              MEA                        
000250     03 MEA-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 032               
000503*                                                                         
000515     03 MEA-6311-MEASURE-QUAL                  PIC X(03).                 
000516*                                              AAX                        
000517*                                                                         
000518     03 MEA-C502-MEASURE-DET-1.                                           
000519*                                                                         
000520        05 MEA-6313-MEASURE-DIM                PIC X(03).                 
000521*                                                                         
000522     03 MEA-C174-MEASURE-DET-2.                                           
000523*                                                                         
000524        05 MEA-6411-MEASURE-UNIT-Q             PIC X(08).                 
000526*       AAD=KGM                                                           
000527*       AAC=KGM                                                           
000528*       ABJ=CUM                                                           
000529*                                                                         
000530        05 MEA-6314-MEASURE-VALUE              PIC 9(18).                 
000532*       AAD= 6.1                                                          
000533*       AAC= 6.3                                                          
000534*       ABJ= 4.3                                                          
000535*                                                                         
000540*** END OF VILMAII-COPY LENGTH=38                                         
