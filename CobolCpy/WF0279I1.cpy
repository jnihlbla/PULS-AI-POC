000100 01  REQU-WF0279I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0279             
000300*                                 VAT MAINTENANCE                         
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDLANDX2-KEY    PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 REQU-KDVAT-KEY       PIC X(2).                                    
001100*                                 MOMSKOD                                 
001200*                                 VAT CODE                                
001300     03 REQU-BEVAT           PIC X(50).                                   
001400*                                 MOMSKODSBENÄMNING R3                    
001500*                                 VAT CODE DESCRIPTION R3                 
001600     03 REQU-REVAT           PIC X(6).                                    
001700*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
001800*                                 VAT FACTOR                              
001900*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
