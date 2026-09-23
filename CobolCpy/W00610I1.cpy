000100 01  REQU-W06610I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM W00610             
000300*                                 START SOP RUTINE IN SOP                 
000400     03 REQU-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 REQU-KDMFSFOR        PIC X.                                       
000800*                                 TYP AV MFS-FORMAT                       
000900*                                 1 = W-FORMAT  2 = N-FORMAT              
001000*                                 TYPE OF MFS FORMAT                      
001100     03 REQU-IDPROCESS       PIC X(10).                                   
001200*                                 PROCESSNAMN                             
001300*                                 PROCESS NAME                            
001400     03 REQU-KDSOPFUNK       PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600*                                 MFS DISPOSITION OF INPUT FIELD          
001700     03 REQU-TESYMBV         PIC X(500).                                  
001800*** END OF VILMAII-COPY LENGTH= 517 BYTES                                 
