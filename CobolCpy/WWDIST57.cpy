000010*** EDIT ALLOWED                                                          
000011*                            *************************************        
000012*                            *** ANVÄNDS VID TEST AV:                     
000013*                            ***  - REFILLDISTRIKT OCH TRANSFER-          
000014*                            ***    DISTRIKT FÖR ATT SÖKA FRAM            
000015*                            ***    MOTTAGANDE DC.                        
000016*                            ***                                          
000017*                            ***    ANVÄND SEARCH ALL FÖR ATT             
000018*                            ***    SÖKA I TABELLEN.                      
000019*                            ***                                          
000020*                            ***  OBS!!                                   
000021*                            ***    TABELLVÄRDENA MÅSTE STÄMMA            
000022*                            ***    MED INNEHÅLLET I WWDIST35.            
000023*                            ***    JUSTERA BÅDA SAMTIDIGT.               
000024*                            ***  OBS!!!                                  
000025*                            ***    TABELLEN MÅSTE VARA SORTERAD          
000026*                            ***    I DISTRIKTS-ORDNING.                  
000027*                            ***    ANPASSA OCCURS NN LÄNGST NER!!        
000028*                            *************************************        
000029*                                                                         
000030*   TABELL FÖR ATT SÖKA DC FÖR REFILL OCH TRANSFER DISTRIKT.              
000040*                                                                         
000050 01  DIST57-TABELL-VALUES.                                                
000060     03  FILLER             PIC X(8) VALUE '08032 21'.                    
000070     03  FILLER             PIC X(8) VALUE '08042 22'.                    
000080     03  FILLER             PIC X(8) VALUE '08052 23'.                    
000090     03  FILLER             PIC X(8) VALUE '08062 24'.                    
000100     03  FILLER             PIC X(8) VALUE '08072 25'.                    
000200     03  FILLER             PIC X(8) VALUE '08082 26'.                    
000300     03  FILLER             PIC X(8) VALUE '08092 3A'.                    
000400     03  FILLER             PIC X(8) VALUE '08141 41'.                    
000500     03  FILLER             PIC X(8) VALUE '08142 42'.                    
000600     03  FILLER             PIC X(8) VALUE '08143 43'.                    
000700     03  FILLER             PIC X(8) VALUE '08144 44'.                    
000800     03  FILLER             PIC X(8) VALUE '08145 45'.                    
000900     03  FILLER             PIC X(8) VALUE '08146 46'.                    
000910     03  FILLER             PIC X(8) VALUE '08147 47'.                    
001000     03  FILLER             PIC X(8) VALUE '08151 51'.                    
001100     03  FILLER             PIC X(8) VALUE '08152 52'.                    
001200     03  FILLER             PIC X(8) VALUE '08153 53'.                    
001210     03  FILLER             PIC X(8) VALUE '08161 61'.                    
001211     03  FILLER             PIC X(8) VALUE '08162 62'.                    
001212     03  FILLER             PIC X(8) VALUE '08163 63'.                    
001213     03  FILLER             PIC X(8) VALUE '08164 64'.                    
001214     03  FILLER             PIC X(8) VALUE '08165 65'.                    
001215     03  FILLER             PIC X(8) VALUE '08166 66'.                    
001216     03  FILLER             PIC X(8) VALUE '08167 67'.                    
001217     03  FILLER             PIC X(8) VALUE '08171 71'.                    
001218     03  FILLER             PIC X(8) VALUE '08172 72'.                    
001219     03  FILLER             PIC X(8) VALUE '08173 73'.                    
001220     03  FILLER             PIC X(8) VALUE '08174 74'.                    
001221     03  FILLER             PIC X(8) VALUE '08181 81'.                    
001222     03  FILLER             PIC X(8) VALUE '08182 82'.                    
001223     03  FILLER             PIC X(8) VALUE '08185 85'.                    
001224     03  FILLER             PIC X(8) VALUE '08186 86'.                    
001225     03  FILLER             PIC X(8) VALUE '08187 87'.                    
001226     03  FILLER             PIC X(8) VALUE '08193 93'.                    
001227     03  FILLER             PIC X(8) VALUE '08263 6A'.                    
001228     03  FILLER             PIC X(8) VALUE '08271 71'.                    
001229     03  FILLER             PIC X(8) VALUE '08272 72'.                    
001230     03  FILLER             PIC X(8) VALUE '08273 73'.                    
001231     03  FILLER             PIC X(8) VALUE '08274 74'.                    
001232     03  FILLER             PIC X(8) VALUE '08293 93'.                    
001233     03  FILLER             PIC X(8) VALUE '08320 51'.                    
001234     03  FILLER             PIC X(8) VALUE '08321 41'.                    
001235     03  FILLER             PIC X(8) VALUE '08323 43'.                    
001236     03  FILLER             PIC X(8) VALUE '08324 44'.                    
001237     03  FILLER             PIC X(8) VALUE '08325 45'.                    
001238     03  FILLER             PIC X(8) VALUE '08326 46'.                    
001239     03  FILLER             PIC X(8) VALUE '08327 47'.                    
001240     03  FILLER             PIC X(8) VALUE '08330 51'.                    
001241     03  FILLER             PIC X(8) VALUE '08331 41'.                    
001242     03  FILLER             PIC X(8) VALUE '08333 43'.                    
001243     03  FILLER             PIC X(8) VALUE '08334 44'.                    
001250     03  FILLER             PIC X(8) VALUE '08335 45'.                    
001260     03  FILLER             PIC X(8) VALUE '08336 46'.                    
001261     03  FILLER             PIC X(8) VALUE '08337 47'.                    
001270     03  FILLER             PIC X(8) VALUE '08341 41'.                    
001280     03  FILLER             PIC X(8) VALUE '08342 42'.                    
001290     03  FILLER             PIC X(8) VALUE '08343 43'.                    
001300     03  FILLER             PIC X(8) VALUE '08344 44'.                    
001400     03  FILLER             PIC X(8) VALUE '08345 45'.                    
001410     03  FILLER             PIC X(8) VALUE '08346 46'.                    
001411     03  FILLER             PIC X(8) VALUE '08347 47'.                    
001412     03  FILLER             PIC X(8) VALUE '08361 61'.                    
001413     03  FILLER             PIC X(8) VALUE '08362 62'.                    
001414     03  FILLER             PIC X(8) VALUE '08363 61'.                    
001415     03  FILLER             PIC X(8) VALUE '08371 71'.                    
001416     03  FILLER             PIC X(8) VALUE '08372 72'.                    
001417     03  FILLER             PIC X(8) VALUE '08373 73'.                    
001418     03  FILLER             PIC X(8) VALUE '08374 74'.                    
001419     03  FILLER             PIC X(8) VALUE '08392 92'.                    
001420     03  FILLER             PIC X(8) VALUE '08393 63'.                    
001421     03  FILLER             PIC X(8) VALUE '08500 6A'.                    
001422     03  FILLER             PIC X(8) VALUE '08541 41'.                    
001430     03  FILLER             PIC X(8) VALUE '08542 42'.                    
001440     03  FILLER             PIC X(8) VALUE '08543 43'.                    
001450     03  FILLER             PIC X(8) VALUE '08544 44'.                    
001460     03  FILLER             PIC X(8) VALUE '08551 51'.                    
001470     03  FILLER             PIC X(8) VALUE '08563 63'.                    
001471     03  FILLER             PIC X(8) VALUE '08564 64'.                    
001472     03  FILLER             PIC X(8) VALUE '08566 66'.                    
001473     03  FILLER             PIC X(8) VALUE '08661 61'.                    
001474     03  FILLER             PIC X(8) VALUE '08662 62'.                    
001475     03  FILLER             PIC X(8) VALUE '08666 66'.                    
001476     03  FILLER             PIC X(8) VALUE '08571 71'.                    
001480     03  FILLER             PIC X(8) VALUE '08572 72'.                    
001490     03  FILLER             PIC X(8) VALUE '08573 73'.                    
001500     03  FILLER             PIC X(8) VALUE '08574 74'.                    
001600     03  FILLER             PIC X(8) VALUE '08700 1A'.                    
001700     03  FILLER             PIC X(8) VALUE '08701 1B'.                    
001800     03  FILLER             PIC X(8) VALUE '08702 1C'.                    
001900     03  FILLER             PIC X(8) VALUE '08703 1D'.                    
001901     03  FILLER             PIC X(8) VALUE '08704 1E'.                    
001902     03  FILLER             PIC X(8) VALUE '08705 1F'.                    
001903     03  FILLER             PIC X(8) VALUE '08706 1G'.                    
001904     03  FILLER             PIC X(8) VALUE '08720 1K'.                    
001905     03  FILLER             PIC X(8) VALUE '08721 41'.                    
001906     03  FILLER             PIC X(8) VALUE '08723 43'.                    
001907     03  FILLER             PIC X(8) VALUE '08724 44'.                    
001908     03  FILLER             PIC X(8) VALUE '08725 45'.                    
001909     03  FILLER             PIC X(8) VALUE '08726 46'.                    
001910     03  FILLER             PIC X(8) VALUE '08727 47'.                    
001911     03  FILLER             PIC X(8) VALUE '08731 41'.                    
001912     03  FILLER             PIC X(8) VALUE '08733 43'.                    
001913     03  FILLER             PIC X(8) VALUE '08734 44'.                    
001914     03  FILLER             PIC X(8) VALUE '08735 45'.                    
001915     03  FILLER             PIC X(8) VALUE '08736 46'.                    
001916     03  FILLER             PIC X(8) VALUE '08737 47'.                    
001917     03  FILLER             PIC X(8) VALUE '08741 41'.                    
001918     03  FILLER             PIC X(8) VALUE '08742 42'.                    
001919     03  FILLER             PIC X(8) VALUE '08743 43'.                    
001920     03  FILLER             PIC X(8) VALUE '08744 44'.                    
001921     03  FILLER             PIC X(8) VALUE '08745 45'.                    
001922     03  FILLER             PIC X(8) VALUE '08746 46'.                    
001923     03  FILLER             PIC X(8) VALUE '08747 47'.                    
001930     03  FILLER             PIC X(8) VALUE '08751 51'.                    
001940     03  FILLER             PIC X(8) VALUE '08771 71'.                    
001950     03  FILLER             PIC X(8) VALUE '08772 72'.                    
001960     03  FILLER             PIC X(8) VALUE '08773 73'.                    
001970     03  FILLER             PIC X(8) VALUE '08775 71'.                    
001980     03  FILLER             PIC X(8) VALUE '08776 72'.                    
001990     03  FILLER             PIC X(8) VALUE '08777 73'.                    
002000     03  FILLER             PIC X(8) VALUE '08792 92'.                    
002100     03  FILLER             PIC X(8) VALUE '08800 2A'.                    
002200     03  FILLER             PIC X(8) VALUE '08801 2B'.                    
002300     03  FILLER             PIC X(8) VALUE '08802 2C'.                    
002301     03  FILLER             PIC X(8) VALUE '08803 2D'.                    
002302     03  FILLER             PIC X(8) VALUE '08804 2E'.                    
002303     03  FILLER             PIC X(8) VALUE '08805 2F'.                    
002304     03  FILLER             PIC X(8) VALUE '08806 2G'.                    
002305     03  FILLER             PIC X(8) VALUE '08807 2H'.                    
002306     03  FILLER             PIC X(8) VALUE '08808 2I'.                    
002307     03  FILLER             PIC X(8) VALUE '08809 2J'.                    
002308     03  FILLER             PIC X(8) VALUE '08810 2K'.                    
002309     03  FILLER             PIC X(8) VALUE '08811 2L'.                    
002310     03  FILLER             PIC X(8) VALUE '08812 2M'.                    
002311     03  FILLER             PIC X(8) VALUE '08813 2N'.                    
002312     03  FILLER             PIC X(8) VALUE '08814 2O'.                    
002313     03  FILLER             PIC X(8) VALUE '08851 3B'.                    
002314     03  FILLER             PIC X(8) VALUE '08852 3C'.                    
002315     03  FILLER             PIC X(8) VALUE '08853 3D'.                    
002316     03  FILLER             PIC X(8) VALUE '08854 3E'.                    
002317     03  FILLER             PIC X(8) VALUE '08855 3F'.                    
002318     03  FILLER             PIC X(8) VALUE '08856 3G'.                    
002319     03  FILLER             PIC X(8) VALUE '08857 3H'.                    
002320     03  FILLER             PIC X(8) VALUE '08858 3I'.                    
002321     03  FILLER             PIC X(8) VALUE '08859 3J'.                    
002322     03  FILLER             PIC X(8) VALUE '08860 3K'.                    
002323     03  FILLER             PIC X(8) VALUE '08861 3L'.                    
002324     03  FILLER             PIC X(8) VALUE '08862 3M'.                    
002325     03  FILLER             PIC X(8) VALUE '08863 3N'.                    
002326     03  FILLER             PIC X(8) VALUE '08864 3O'.                    
002327     03  FILLER             PIC X(8) VALUE '08865 3P'.                    
002328     03  FILLER             PIC X(8) VALUE '08866 3R'.                    
002329     03  FILLER             PIC X(8) VALUE '08867 3S'.                    
002330     03  FILLER             PIC X(8) VALUE '08868 3T'.                    
002331     03  FILLER             PIC X(8) VALUE '08871 71'.                    
002332     03  FILLER             PIC X(8) VALUE '08872 72'.                    
002333     03  FILLER             PIC X(8) VALUE '08873 73'.                    
002334     03  FILLER             PIC X(8) VALUE '08874 74'.                    
002335     03  FILLER             PIC X(8) VALUE '08900 7A'.                    
002336     03  FILLER             PIC X(8) VALUE '08901 7B'.                    
002337     03  FILLER             PIC X(8) VALUE '08902 7C'.                    
002338     03  FILLER             PIC X(8) VALUE '08903 7D'.                    
002339     03  FILLER             PIC X(8) VALUE '08904 7E'.                    
002340     03  FILLER             PIC X(8) VALUE '08905 7F'.                    
002341     03  FILLER             PIC X(8) VALUE '08906 7G'.                    
002342     03  FILLER             PIC X(8) VALUE '08907 7H'.                    
002343     03  FILLER             PIC X(8) VALUE '08950 7A'.                    
002344     03  FILLER             PIC X(8) VALUE '08951 7B'.                    
002345     03  FILLER             PIC X(8) VALUE '08952 7C'.                    
002346     03  FILLER             PIC X(8) VALUE '08953 7D'.                    
002347     03  FILLER             PIC X(8) VALUE '08954 7E'.                    
002348     03  FILLER             PIC X(8) VALUE '08955 7F'.                    
002349     03  FILLER             PIC X(8) VALUE '08956 7G'.                    
002350     03  FILLER             PIC X(8) VALUE '08957 7H'.                    
002351     03  FILLER             PIC X(8) VALUE '08960 7A'.                    
002352     03  FILLER             PIC X(8) VALUE '08961 7B'.                    
002353     03  FILLER             PIC X(8) VALUE '08962 7C'.                    
002354     03  FILLER             PIC X(8) VALUE '08963 7D'.                    
002355     03  FILLER             PIC X(8) VALUE '08964 7E'.                    
002356     03  FILLER             PIC X(8) VALUE '08965 7F'.                    
002357     03  FILLER             PIC X(8) VALUE '08966 7G'.                    
002358     03  FILLER             PIC X(8) VALUE '08967 7H'.                    
002359     03  FILLER             PIC X(8) VALUE '08970 7A'.                    
002360     03  FILLER             PIC X(8) VALUE '08971 7B'.                    
002361     03  FILLER             PIC X(8) VALUE '08972 7C'.                    
002362     03  FILLER             PIC X(8) VALUE '08973 7D'.                    
002363     03  FILLER             PIC X(8) VALUE '08974 7E'.                    
002364     03  FILLER             PIC X(8) VALUE '08975 7F'.                    
002365     03  FILLER             PIC X(8) VALUE '08976 7G'.                    
002366     03  FILLER             PIC X(8) VALUE '08977 7H'.                    
002367     03  FILLER             PIC X(8) VALUE '09111 11'.                    
002368     03  FILLER             PIC X(8) VALUE '09141 41'.                    
002369     03  FILLER             PIC X(8) VALUE '09143 43'.                    
002370     03  FILLER             PIC X(8) VALUE '09144 44'.                    
002371     03  FILLER             PIC X(8) VALUE '09145 45'.                    
002372     03  FILLER             PIC X(8) VALUE '09146 46'.                    
002373     03  FILLER             PIC X(8) VALUE '09147 47'.                    
002373     03  FILLER             PIC X(8) VALUE '09153 53'.                    
002374     03  FILLER             PIC X(8) VALUE '09161 61'.                    
002375     03  FILLER             PIC X(8) VALUE '09162 62'.                    
002376     03  FILLER             PIC X(8) VALUE '09163 63'.                    
002377     03  FILLER             PIC X(8) VALUE '09164 64'.                    
002378     03  FILLER             PIC X(8) VALUE '09165 65'.                    
002379     03  FILLER             PIC X(8) VALUE '09166 66'.                    
002380     03  FILLER             PIC X(8) VALUE '09167 67'.                    
002381     03  FILLER             PIC X(8) VALUE '09181 81'.                    
002382     03  FILLER             PIC X(8) VALUE '09182 82'.                    
002383     03  FILLER             PIC X(8) VALUE '09183 83'.                    
002384     03  FILLER             PIC X(8) VALUE '09184 84'.                    
002385     03  FILLER             PIC X(8) VALUE '09185 85'.                    
002386     03  FILLER             PIC X(8) VALUE '09186 86'.                    
002387     03  FILLER             PIC X(8) VALUE '09187 87'.                    
002388     03  FILLER             PIC X(8) VALUE '09193 93'.                    
002389     03  FILLER             PIC X(8) VALUE '09211 11'.                    
002390     03  FILLER             PIC X(8) VALUE '09261 61'.                    
002391     03  FILLER             PIC X(8) VALUE '09262 62'.                    
002392     03  FILLER             PIC X(8) VALUE '09263 63'.                    
002393     03  FILLER             PIC X(8) VALUE '09264 64'.                    
002394     03  FILLER             PIC X(8) VALUE '09265 65'.                    
002395     03  FILLER             PIC X(8) VALUE '09266 66'.                    
002396     03  FILLER             PIC X(8) VALUE '09267 67'.                    
002397     03  FILLER             PIC X(8) VALUE '09271 71'.                    
002398     03  FILLER             PIC X(8) VALUE '09272 72'.                    
002399     03  FILLER             PIC X(8) VALUE '09273 73'.                    
002400     03  FILLER             PIC X(8) VALUE '09274 74'.                    
002401     03  FILLER             PIC X(8) VALUE '09281 81'.                    
002402     03  FILLER             PIC X(8) VALUE '09282 82'.                    
002403     03  FILLER             PIC X(8) VALUE '09283 83'.                    
002404     03  FILLER             PIC X(8) VALUE '09284 84'.                    
002405     03  FILLER             PIC X(8) VALUE '09285 85'.                    
002406     03  FILLER             PIC X(8) VALUE '09286 86'.                    
002407     03  FILLER             PIC X(8) VALUE '09287 87'.                    
002408     03  FILLER             PIC X(8) VALUE '09361 61'.                    
002409     03  FILLER             PIC X(8) VALUE '09362 62'.                    
002409     03  FILLER             PIC X(8) VALUE '09364 64'.                    
002409     03  FILLER             PIC X(8) VALUE '09365 65'.                    
002410     03  FILLER             PIC X(8) VALUE '09366 66'.                    
002411     03  FILLER             PIC X(8) VALUE '09367 67'.                    
002412     03  FILLER             PIC X(8) VALUE '09465 65'.                    
002413     03  FILLER             PIC X(8) VALUE '09467 67'.                    
002414     03  FILLER             PIC X(8) VALUE '09471 71'.                    
002417*                                                                         
002418 01  DIST57-REFILL-DC-TAB REDEFINES DIST57-TABELL-VALUES.                 
002419     03  DIST57-REFILL-DC OCCURS 239 TIMES                                
002420                          ASCENDING KEY IS DIST57-SOK-IDDISTR             
002430                          INDEXED BY DIST57-IX.                           
002440       05  DIST57-SOK-IDDISTR  PIC 9(5).                                  
002450       05  FILLER              PIC X(1).                                  
002460       05  DIST57-REFILL-TO-DC PIC X(2).                                  
002470*                                                                         
