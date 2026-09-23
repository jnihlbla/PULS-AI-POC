000010 01  W4266803.                                                            
000020*                                 UPPFÖLJNINGSREG.                        
000030*                                 PÅ HISTORIK FIL                         
000040     03 IDPTYP               PIC X(3).                                    
000050*                                 POSTTYP                                 
000060     03 IDKVAINF             PIC 9(2).                                    
000070*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000080     03 KDKVASTA-PRI         PIC X.                                       
000090*                                 STATUS PRIMÄRKONTROLL                   
000100     03 TEKVAINF             OCCURS 3 TIMES                               
000110                             PIC X(60).                                   
000120*                                 KVALITETS INFORMATION                   
      *** END COPY W4266803    LENGTH=186                                       
