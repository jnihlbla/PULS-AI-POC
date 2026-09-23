000100 01  REQU-WZ0430I1.                                                       
000200     03 REQU-IDMSGVER        PIC 9(3).                                    
000300*                                 VERSION NUMBER OF THE MESSAGE           
000400     03 REQU-IDEVENT         PIC X(30).                                   
000500*                                 EVENT NAME                              
000600     03 REQU-IDEVENTREC      PIC X(30).                                   
000700*                                 EVENT RECEIVER                          
000800     03 REQU-IDEVENTTYP      PIC X(10).                                   
000900*                                 EVENT TYPE                              
001000     03 REQU-TIMESTAMP       PIC X(26).                                   
001100     03 REQU-IDCPYTXT.                                                    
001200*                                 IDENTITY OF A COPYTEXT                  
001300        05 REQU-CT-IDSYSTEM  PIC X(4).                                    
001400*                                 VOLVO VCCS SYSTEM NUMBER                
001500        05 REQU-CT-IDPTYP    PIC X(3).                                    
001600*                                 RECORD TYPE                             
001700        05 REQU-CT-IDVTYP    PIC X.                                       
001800*                                 RECORD TYPE VERSION                     
001900     03 REQU-EVENT-DATA      PIC X(500).                                  
002000*** END OF VILMAII-COPY LENGTH= 607 BYTES                                 
