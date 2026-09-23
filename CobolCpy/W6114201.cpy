000010 01  W61142.                                                              
000020*                                 URVAL FRÅN W6G3                         
000030*                                 MED DAGENS HÄNDELSER                    
000040*                                                                         
000050     03 TIREGDAT             PIC S9(7)           COMP-3.                  
000060*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000070     03 TIKLOCK              PIC S9(9)           COMP-3.                  
000080*                                 KLOCKSLAG (TTMMSSTH)                    
000090     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000110*                                 (0VVDLLLLK)                             
000120     03 IDRADNR              PIC S9(5)           COMP-3.                  
000130*                                 RADNUMMER                               
000140     03 KDINLPRIO            PIC S9(3)           COMP-3.                  
000150*                                 PRIORITETSGRUPP                         
000160     03 KDINLSTA             PIC X(3).                                    
000170*                                 SYSTEMSTATUS INLEVERANS                 
000180     03 KDINLUPF             PIC X(4).                                    
000190*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
000200     03 KDINLUPF-NXT         PIC X(4).                                    
000210*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
000220*                                 RANS                                    
000230     03 KVINLART             PIC S9(7)           COMP-3.                  
000240*                                 ANTAL I PARTIRAD                        
000250     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
000260*                                 ARTIKELSTANDARDPRIS                     
      *** END COPY W6114201    LENGTH=39                                        
