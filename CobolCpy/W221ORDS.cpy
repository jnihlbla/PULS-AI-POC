000010*** EDIT ALLOWED                                                          
000100************************************************************              
000200*                                                                         
000210*    W221ORDS   ORDERSÄRKOSTNAD                                           
000220*                                                                         
000300*    KONSTANTVÄRDEN FÖR BERÄKNING AV ORDERSÄRKOSTNAD.                     
000400*                                                                         
000500*    ANVÄNDS AV PROGRAM W22170.                                           
000700*                                                                         
001100************************************************************              
001200                                                                          
001300 01  ORDERSARKOSTNAD.                                                     
001310   03 CALL-OFF.                                                           
001401     05  ORDS-ODETTE-AVROP  PIC S9(5)V9(2) COMP-3 VALUE 00010.00.         
001402     05  ORDS-FAXAVROP      PIC S9(5)V9(2) COMP-3 VALUE 00020.00.         
001403     05  ORDS-EJ-CALL-OFF   PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001404   03 FORAVISERING.                                                       
001406     05  ORDS-ODETTE-FORAVI PIC S9(5)V9(2) COMP-3 VALUE 00020.00.         
001407     05  ORDS-MAN-FORAVIS   PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001408   03 FAKTURA.                                                            
001409     05  ORDS-ODETTE-BEHAND PIC S9(5)V9(2) COMP-3 VALUE 00005.00.         
001410     05  ORDS-ODETTE-MOTTAG PIC S9(5)V9(2) COMP-3 VALUE 00020.00.         
001411     05  ORDS-MAN-BEHAND    PIC S9(5)V9(2) COMP-3 VALUE 00030.00.         
001412   03 TULL.                                                               
001413     05  ORDS-SVENSK-LEV    PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001414     05  ORDS-EJSVENSK-LEV  PIC S9(5)V9(2) COMP-3 VALUE 00120.00.         
001415   03 MOTTAGNING.                                                         
001416     05  ORDS-ODETTE-FLAGGA PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001417     05  ORDS-MAN-FLAGGA    PIC S9(5)V9(2) COMP-3 VALUE 00100.00.         
001418   03 MOTTAGNINGSREGISTRERING.                                            
001419     05  ORDS-REG-FORAVIS   PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001420     05  ORDS-REG-EJ-FORAVI PIC S9(5)V9(2) COMP-3 VALUE 00025.00.         
001421   03 KVALITETSKONTROLL.                                                  
001422     05  ORDS-KVAL-KONTR    PIC S9(5)V9(2) COMP-3 VALUE 00215.00.         
001423     05  ORDS-EJ-KVAL-KONTR PIC S9(5)V9(2) COMP-3 VALUE 00000.00.         
001424   03 EFTERBEHANDLING.                                                    
001425     05  ORDS-STALLT-PACKN  PIC S9(5)V9(2) COMP-3 VALUE 00065.00.         
001430     05  ORDS-STALLT-MALN   PIC S9(5)V9(2) COMP-3 VALUE 00065.00.         
