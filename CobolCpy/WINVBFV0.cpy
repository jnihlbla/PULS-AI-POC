000100*** EDIT ALLOWED                                                          
000200 01  WDIRBFV0.                                                            
000300*                                 DIRECT BUSINESS,                        
000400*                                 FEEDBACK RECORD.                        
000500*                                                                         
000600     03 V0-PROGRAM        PIC X(4).                                       
000700*                                                                         
000800     03 V0-FILEDEFNO      PIC X(2).                                       
000900*                                                                         
001000     03 V0-SENDLOC        PIC X(3).                                       
001100*                                                                         
001200     03 V0-RECLOC         PIC X(3).                                       
001300*                                                                         
001400     03 V0-SERIALNO       PIC X(7).                                       
001500*                                                                         
001600     03 V0-RECORDTYPE     PIC X(1).                                       
001700*                                                                         
001800*                                                                         
001900     03 V0-ORIG-BILL-DATA PIC X(155).                                     
002000*                                                                         
002010     03 V0-VALIDITY-IND   PIC X(1).                                       
002110*                                                                         
002111*                                                                         
002120     03 V0-FEEDB-DATA     PIC X(48).                                      
002121*                                                                         
002130     03 V0-VALID-FEEDB REDEFINES V0-FEEDB-DATA.                           
002200*                                                                         
002500       05 V0-INVNO        PIC X(10).                                      
002600*                                                                         
002610       05 V0-ACC-PER      PIC 9(6).                                       
002620*                                                                         
002630       05 V0-LINE-VALUE   PIC X(11).                                      
002640*                                                                         
002650       05 V0-VAT-CODE     PIC X(1).                                       
002660*                                                                         
002670       05 V0-VAT-PERCENT  PIC 9(5).                                       
002680*                                                                         
002692       05 FILLER          PIC X(15).                                      
002693*                                                                         
002694     03 V0-INVALID-FEEDB REDEFINES V0-FEEDB-DATA.                         
002695*                                                                         
002696       05 V0-ERROR-CODE   OCCURS 12                                       
002697                          PIC X(3).                                       
002698*                                                                         
002699       05 FILLER          PIC X(12).                                      
002700*                                                                         
002901*                                                                         
008700*** END OF VILMAII-COPY LENGTH=224                                        
