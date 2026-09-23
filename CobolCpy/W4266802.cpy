000010 01  W4266802.                                                            
000020*                                 UPPFÖLJNINGSREG.                        
000030*                                 PÅ HISTORIK FIL                         
000040     03 IDPTYP               PIC X(3).                                    
000050*                                 POSTTYP                                 
000060     03 IDKR                 PIC 9(5).                                    
000070*                                 KONTROLLRAPPORT NUMMER                  
000080     03 BEKRFEL              OCCURS 3 TIMES                               
000090                             PIC X(70).                                   
000100*                                 BESKRIVNING FELKOD KONTR.RAPPOR         
000110*                                 T                                       
000120     03 KDKVASTA-PRI         PIC X.                                       
000130*                                 STATUS PRIMÄRKONTROLL                   
000140     03 TEKRFEL              PIC X(70).                                   
000150*                                 FELBESKRIVNING I FRI TEXT               
000160*                                                                         
      *** END COPY W4266802    LENGTH=289                                       
