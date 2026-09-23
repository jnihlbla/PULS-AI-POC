000010 01  4302-WDGX4302.                                                       
000020*                                 LÅSNINGSREGISTER                        
000030*                                 NYCKEL:  WDGXKEY                        
000040*                                          (IDKOLLI,IDPLKLST)             
000050     03 4302-IDKOLLI         PIC S9(5)           COMP-3.                  
000060*                                 KOLLINUMMER                             
000070     03 4302-IDPLKLST        PIC S9(3)           COMP-3.                  
000080*                                 PLOCKLISTNUMMER                         
000090     03 4302-KDKOLSTA        PIC S9              COMP-3.                  
000100*                                 KOLLISTATUS                             
000110     03 4302-FLBANDST        PIC X.                                       
000120*                                 BANDSTATIONSFLAGGA                      
000130     03 4302-FILLER          PIC X(13).                                   
      *** END COPY WDGX4302    LENGTH=20                                        
