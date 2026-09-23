000100 01  PRMT-W510PRMT.                                                       
000200*                                 LINKAGE AREA ON CALL OF SUBPGM          
000300*                                                                         
000400*                                 W510PRMT.                               
000500*                                                                         
000600*                                                                         
000700*                                                                         
000800     03 PRMT-KDCALL          PIC S9(3)           COMP-3.                  
000900*                                 ANROPSTYP                               
001000     03 PRMT-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 PRMT-KDSVAR          PIC X.                                       
001300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001400     03 PRMT-W510PRMT-RESP   OCCURS 50 TIMES.                             
001500        05 PRMT-IDDC         PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
