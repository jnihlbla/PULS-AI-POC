000100*** EDIT ALLOWED                                                          
000200*                            *************************************        
000300*                            *** ANVÄNDS FÖR ATT KONTROLLERA              
000400*                            *** PRODUKTKODER.                            
000500*                            *** I TRANSAR FÖR DIRECT-BUSINESS.           
000600*                            *** I TEST INGÅR PROGRAM OCH KOD.            
000700*                            *************************************        
000800*                                                                         
000900*                                                                         
001000 01  W463PROD-TEST           PIC X(5).                                    
001100*                                                                         
001200     88  W463PROD-BC-OK      VALUE 'VCON0' 'VCON1' 'VCON2'                
001300                                   'VCON3' 'VCON6'                        
001400                                   'VMER0' 'VMER6' 'VMER9'                
001500                                   'VSER6' 'VSER7'                        
001600                                   'VTYR1' 'VTYR2' 'VTYR4'                
001700                                   'VTYR5' 'VTYR8' 'VTYR9'                
001800                                   'VTYRE'                                
001900                                   'VTYRF' 'VTYRG' 'VTYRH'.               
002000                                                                          
003000     88  W463PROD-D-OK       VALUE 'VCON2'                                
003100                                   'VCONA' 'VCONB' 'VCONC'                
004000                                   'VMERA' 'VMERB' 'VMERC'                
005000                                   'VTYRA' 'VTYRB' 'VTYRC'.               
006000                                                                          
007000     88  W463PROD-P-OK       VALUE 'VTYR1' 'VTYR2' 'VTYR4'                
008000                                   'VTYR5' 'VTYR8' 'VTYR9'                
009000                                   'VTYRE'                                
010000                                   'VTYRF' 'VTYRG' 'VTYRH'.               
020000                                                                          
030000     88  W463PROD-KB         VALUE 'VCON0' 'VCON1' 'VCON2'                
040000                                   'VCON3' 'VCON6'                        
050000                                   'VMER0' 'VMER6' 'VMER9'                
060000                                   'VSER6' 'VSER7'                        
070000                                   'VTYR1' 'VTYR2' 'VTYR4'                
080000                                   'VTYR5' 'VTYR8' 'VTYR9'                
090000                                   'VTYRE'                                
100000                                   'VTYRF' 'VTYRG' 'VTYRH'.               
110000                                                                          
120000*                                                                         
130000*** END COPY W463PROD    LENGTH=5                                         
