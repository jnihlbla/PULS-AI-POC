000100 01  SPEC-W6L112.                                                         
000200*                                 UPPFÖLJINGNSREGISTER                    
000300*                                 SPECIALKONTROLL                         
000400*                                 FYSISK NYCKEL: IDKVAINF                 
000500     03 SPEC-IDKVAINF        PIC 9(2).                                    
000600*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
000700*                                 LINENO FOR QUALITY CONTROL TEXT         
000800     03 SPEC-KDKVASTA-PRI    PIC X.                                       
000900*                                 STATUS PRIMÄRKONTROLL                   
001000*                                 STATUS PRIMARY INSPECTION               
001100     03 SPEC-TEKVAINF        OCCURS 3 TIMES                               
001200                             PIC X(60).                                   
001300*                                 KVALITETS INFORMATION                   
001400*                                 QUALITY INFORMATION PART NUMBER         
001500     03 SPEC-IDUSER          PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800*** END OF VILMAII-COPY LENGTH= 191 BYTES                                 
