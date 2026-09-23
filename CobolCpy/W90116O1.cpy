000100 01  W90116O1-CTX.                                                        
000200*                                 COPYTEXT TILL PROGRAM W90116O0          
000300*                                 KDSVAR   "Y"  = YES                     
000400*                                          "N"  = NO                      
000500*                                          "E"  = INPUT ERROR             
000600*                                                                         
000700     03 KDSVAR               PIC X.                                       
000800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 FLDIRLEV             PIC X.                                       
001200*                                 DIREKTLEVERANS ?                        
001300     03 TIDISPIN             PIC X(6).                                    
001400*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
001500     03 FLERSATT             PIC X.                                       
001600*                                 ERSATT I VIPS                           
001700*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
