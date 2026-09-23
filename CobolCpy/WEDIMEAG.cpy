000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI MEA GOODS ITEM DETAILS                                           
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO SPECIFY PSYSICAL MEASUREMENS, INCLUDING DIMENSIONS               
      *     TOLERANCE, WEIGHTS AND COUNTS                                       
000036*                                                                         
000040*                                                                         
000100 01  WEDIMEA.                                                             
000230     03 MEA-IDPTYP                             PIC X(03).                 
000240*                                              MEA                        
000501     03 MEA-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 144               
000503*                                                                         
000515     03 MEA-6311-MEASURE-QUAL                  PIC X(03).                 
000516*                                                                         
000517     03 MEA-C502-MEASURE-DET.                                             
000518*                                                                         
000519        05 MEA-6313-MEASURE-DIM                PIC X(03).                 
000518*                                                                         
000519        05 MEA-6321-MEASURE-SIG                PIC X(03).                 
000518*                                                                         
000519        05 MEA-6155-MEASURE-ATTR               PIC X(03).                 
000522*                                                                         
000519        05 MEA-6154                            PIC X(70).                 
000522*                                                                         
000523     03 MEA-C174-VALUE-RANGE.                                             
000524*                                                                         
000525        05 MEA-6411-MEASURE-UNIT-Q             PIC X(03).                 
000526*                                                                         
000527        05 MEA-6314-MEASURE-VALUE              PIC 9(18).                 
000530*                                                                         
000540        05 MEA-6162-RANGE-MIN                  PIC 9(18).                 
000550*                                                                         
000560        05 MEA-6152-RANGE-MAX                  PIC 9(18).                 
000561*                                                                         
000560        05 MEA-6432                            PIC 9(02).                 
000561*                                                                         
000562     03 MEA-7383-SURFACE-LAYER                 PIC X(03).                 
000563*                                                                         
000570*** END OF VILMAII-COPY LENGTH=150                                        
