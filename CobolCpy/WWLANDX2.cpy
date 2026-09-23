000010*** EDIT ALLOWED                                                          
000020*                            *************************************        
000030*                            *** ANVÄNDS VID TEST AV:                     
000040*                            *** - EU-LÄNDER                              
000050*                            *** - NDC-LÄNDER                             
000060*                            *************************************        
000070*                                                                         
000080 01  LANDX2-IDLANDX2         PIC X(2).                                    
000090*                                                                         
000100     88  LANDX2-EU-IDLANDX2  VALUE 'IE' 'BE' 'NL'                         
000200                                   'LU' 'FR' 'DK' 'DE'                    
000300                                   'IT' 'ES' 'PT' 'GR'                    
000400                                   'SE' 'AT' 'FI' 'PL'                    
000500                                   'EE' 'LV' 'LT' 'CY'                    
000600                                   'MT' 'RO' 'BG' 'HU'                    
000700                                   'SK' 'SI' 'CZ' 'HR'.                   
000800                                                                          
000900     88  LANDX2-EU-EJ-SE     VALUE 'GB' 'IE' 'BE' 'NL'                    
001000                                   'LU' 'FR' 'DK' 'DE'                    
001100                                   'IT' 'ES' 'PT' 'GR'                    
001200                                        'AT' 'FI' 'PL'                    
001300                                   'EE' 'LV' 'LT' 'CY'                    
001400                                   'MT' 'RO' 'BG' 'HU'                    
001500                                   'SK' 'SI' 'CZ' 'HR'.                   
001600                                                                          
001700     88  LANDX2-NDC          VALUE 'AE' 'AU' 'BR' 'CA' 'CN' 'IN'          
001800                                   'JP' 'KR' 'MX' 'MY' 'RU' 'TH'          
001810                                   'TR' 'TW' 'US' 'ZA'.                   
