000100 01  PRTR-W510PRTR.                                                       
000200*                                 LÄNKAREA VID ANROP AV SUBPGM            
000300*                                 W510PRTR.                               
000400*                                                                         
000500     03 PRTR-KDCALL          PIC S9(3)           COMP-3.                  
000600*                                 ANROPSTYP                               
000700     03 PRTR-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 PRTR-IDLEVNR         PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 PRTR-TIPRLIST        PIC S9(7)           COMP-3.                  
001200*                                 PRISLISTEDATUM (AAMMDD)                 
001300     03 PRTR-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 PRTR-KDSVAR          PIC X.                                       
001600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001700*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
