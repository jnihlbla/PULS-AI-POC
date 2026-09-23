000100 01  AREA.                                                                
000200*                                 STYR-PARAMETRAR FÖR                     
000300*                                 KOMMUNIKATION MELLAN W4301400           
000400*                                 OCH DESS IMS-SUBPROGRAM.                
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3                   
000700                             VALUE ZEROS.                                 
000800*                                 ANROPSTYP                               
000900     03 LAS-ART              PIC S9(3)           COMP-3                   
001000                             VALUE +1.                                    
001100*** END COPY W430L140C0  LENGTH=4                                         
