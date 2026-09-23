000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI MEA GOODS ITEN DETAIS                                            
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO IDENTIFY A GOODS ITEM FOR WICH TRANSPORT IS UNDERTAKEN.          
000036*                                                                         
000040*                                                                         
000100 01  WEDIMEA3.                                                            
000230     03 MEA3-IDPTYP                            PIC X(03).                 
000240*                                              MEA                        
000501     03 MEA3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 066               
000503*                                                                         
000515     03 MEA3-6311-MEASURMENT-QUAL              PIC X(03).                 
000516*                                                                         
000517     03 MEA3-C502-MEASURMENT-DETAILS.                                     
000518*                                                                         
000519        05 MEA3-6313-MEASURMENT-DIM            PIC X(03).                 
000522*                                                                         
000523     03 MEA3-C174-VALUE-RANGE.                                            
000524*                                                                         
000525        05 MEA3-6411-MEASURE-UNIT-QUAL         PIC X(03).                 
000526*                                                                         
000527        05 MEA3-6314-MEASURMENT-VALUE          PIC 9(15)V9(03).           
000530*                                                                         
000540        05 MEA3-6162-RANGE-MIN                 PIC 9(18).                 
000550*                                                                         
000560        05 MEA3-6152-RANGE-MAX                 PIC 9(18).                 
000561*                                                                         
000562     03 MEA3-7383-SURFACE-LAYER                PIC X(03).                 
000563*                                                                         
000570*** END OF VILMAII-COPY LENGTH=72                                         
