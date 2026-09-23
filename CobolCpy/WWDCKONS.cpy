000001*** EDIT ALLOWED                                                          
000002******************************************************************        
000003**                                                                        
000004**                     W W D C K O NS                                     
000005**                                                                        
000006**   USAGE :                                                              
000007**                                                                        
000008**         THIS COPY TEXT CONTAINS NECESSARY CONSTANTS FOR ALL            
000009**         VALID DC CODES THAT MUST BE USED IN THE PROGRAMS.              
000010**                                                                        
000011**         AN NEW DC CODE INTRODUCED MUST BE INITIALISED HERE WIT         
000012**         A CONSTANT 'WC-' ITEM.                                         
000013**                                                                        
000014**   IMPORTANT :                                                          
000015**                                                                        
000016**         IF THERE IS ANY CHANGE MADE TO THIS COPYTEXT, THEN ALL         
000017**         PROGRAMS THAT HAS THIS COPYTEXT MUST BE RECOMPILED.            
000018**                                                                        
000019******************************************************************        
000020*                                                                         
000021 01  CONST-IDDC.                                                          
000022*                                                                         
000023     03  WC-DC-ZERO              PIC X(2)   VALUE  '00'.                  
000024*                                                                         
000025     03  WC-CDC-SE               PIC X(2)   VALUE  '11'.                  
000026*                                                                         
000027     03  WC-CDC-TR               PIC X(2)   VALUE  '12'.                  
000028*                                                                         
000029     03  WC-SDC-NL               PIC X(2)   VALUE  '21'.                  
000030*                                                                         
000033     03  WC-SDC-ES               PIC X(2)   VALUE  '24'.                  
000034*                                                                         
000035     03  WC-SDC-IT               PIC X(2)   VALUE  '25'.                  
000036*                                                                         
000037     03  WC-SDC-AT               PIC X(2)   VALUE  '26'.                  
000038*                                                                         
000039     03  WC-NDC-US-RU            PIC X(2)   VALUE  '41'.                  
000040*                                                                         
000043     03  WC-NDC-US-LA            PIC X(2)   VALUE  '43'.                  
000044*                                                                         
000045     03  WC-NDC-US-SE            PIC X(2)   VALUE  '44'.                  
000046*                                                                         
000047     03  WC-NDC-US-CH            PIC X(2)   VALUE  '45'.                  
000048*                                                                         
000049     03  WC-NDC-US-JA            PIC X(2)   VALUE  '46'.                  
000050*                                                                         
000051     03  WC-NDC-US-DA            PIC X(2)   VALUE  '47'.                  
000052*                                                                         
000053     03  WC-NDC-CA               PIC X(2)   VALUE  '51'.                  
000054*                                                                         
000055     03  WC-NDC-BR               PIC X(2)   VALUE  '52'.                  
000056*                                                                         
000057     03  WC-NDC-MX               PIC X(2)   VALUE  '53'.                  
000058*                                                                         
000059     03  WC-NDC-JP-6A            PIC X(2)   VALUE  '6A'.                  
000060*                                                                         
000061     03  WC-NDC-JP-61            PIC X(2)   VALUE  '61'.                  
000062*                                                                         
000063     03  WC-NDC-AU               PIC X(2)   VALUE  '62'.                  
000064*                                                                         
000065     03  WC-NDC-TH               PIC X(2)   VALUE  '63'.                  
000066*                                                                         
000067     03  WC-NDC-TW               PIC X(2)   VALUE  '64'.                  
000068*                                                                         
000069     03  WC-NDC-KR               PIC X(2)   VALUE  '65'.                  
000070*                                                                         
000071     03  WC-NDC-MY               PIC X(2)   VALUE  '66'.                  
000072*                                                                         
000073     03  WC-NDC-IN               PIC X(2)   VALUE  '67'.                  
000074*                                                                         
000075     03  WC-NDC-CN-71            PIC X(2)   VALUE  '71'.                  
000076*                                                                         
000077     03  WC-NDC-CN-72            PIC X(2)   VALUE  '72'.                  
000078*                                                                         
000079     03  WC-NDC-CN-73            PIC X(2)   VALUE  '73'.                  
000080*                                                                         
000081     03  WC-NDC-CN-74            PIC X(2)   VALUE  '74'.                  
000082*                                                                         
000083     03  WC-NDC-RU-81            PIC X(2)   VALUE  '81'.                  
000084*                                                                         
000085     03  WC-NDC-RU-82            PIC X(2)   VALUE  '82'.                  
000086*                                                                         
000087     03  WC-NDC-ZA               PIC X(2)   VALUE  '85'.                  
000088*                                                                         
000089     03  WC-NDC-TR               PIC X(2)   VALUE  '86'.                  
000090*                                                                         
000091     03  WC-NDC-AE               PIC X(2)   VALUE  '87'.                  
000092*                                                                         
000093     03  WC-SDC-NL-ET            PIC X(2)   VALUE  '91'.                  
000094*                                                                         
000095     03  WC-NDC-US-BAT           PIC X(2)   VALUE  '92'.                  
000096*                                                                         
000097     03  WC-NDC-TH-93            PIC X(2)   VALUE  '93'.                  
000098*                                                                         
000099     03  WC-LDC-SE-1A            PIC X(2)   VALUE  '1A'.                  
000100*                                                                         
000101     03  WC-LDC-SE-1B            PIC X(2)   VALUE  '1B'.                  
000102*                                                                         
000103     03  WC-LDC-SE-1C            PIC X(2)   VALUE  '1C'.                  
000104*                                                                         
000105     03  WC-LDC-SE-1D            PIC X(2)   VALUE  '1D'.                  
000106*                                                                         
000107     03  WC-LDC-SE-1E            PIC X(2)   VALUE  '1E'.                  
000108*                                                                         
000109     03  WC-LDC-SE-1F            PIC X(2)   VALUE  '1F'.                  
000110*                                                                         
000111     03  WC-LDC-SE-1G            PIC X(2)   VALUE  '1G'.                  
000112*                                                                         
000113     03  WC-LDC-SE-1K            PIC X(2)   VALUE  '1K'.                  
000114*                                                                         
000115     03  WC-LDC-GB-2C            PIC X(2)   VALUE  '2C'.                  
000116*                                                                         
000119     03  WC-LDC-GB-2H            PIC X(2)   VALUE  '2H'.                  
000120*                                                                         
000121     03  WC-LDC-DE-2I            PIC X(2)   VALUE  '2I'.                  
000122*                                                                         
000123     03  WC-LDC-DE-2J            PIC X(2)   VALUE  '2J'.                  
000124*                                                                         
000127     03  WC-LDC-DE-2L            PIC X(2)   VALUE  '2L'.                  
000128*                                                                         
000129     03  WC-LDC-NL-2M            PIC X(2)   VALUE  '2M'.                  
000130*                                                                         
000131     03  WC-LDC-XX-2N            PIC X(2)   VALUE  '2N'.                  
000132*                                                                         
000133     03  WC-LDC-CH-2O            PIC X(2)   VALUE  '2O'.                  
000134*                                                                         
000135     03  WC-LDC-GB-3A            PIC X(2)   VALUE  '3A'.                  
000136*                                                                         
000137     03  WC-LDC-GB-3B            PIC X(2)   VALUE  '3B'.                  
000138*                                                                         
000139     03  WC-LDC-DE-3C            PIC X(2)   VALUE  '3C'.                  
000140*                                                                         
000141     03  WC-LDC-IT-3D            PIC X(2)   VALUE  '3D'.                  
000142*                                                                         
000143     03  WC-LDC-DE-3E            PIC X(2)   VALUE  '3E'.                  
000144*                                                                         
000145     03  WC-LDC-IT-3F            PIC X(2)   VALUE  '3F'.                  
000146*                                                                         
000147     03  WC-LDC-DE-3G            PIC X(2)   VALUE  '3G'.                  
000148*                                                                         
000149     03  WC-LDC-CH-3H            PIC X(2)   VALUE  '3H'.                  
000150*                                                                         
000153     03  WC-LDC-NO-3J            PIC X(2)   VALUE  '3J'.                  
000154*                                                                         
000155     03  WC-LDC-DE-3K            PIC X(2)   VALUE  '3K'.                  
000156*                                                                         
000157     03  WC-LDC-BE-3L            PIC X(2)   VALUE  '3L'.                  
000158*                                                                         
000159     03  WC-LDC-DE-3M            PIC X(2)   VALUE  '3M'.                  
000160*                                                                         
000161     03  WC-LDC-NL-3N            PIC X(2)   VALUE  '3N'.                  
000162*                                                                         
000163     03  WC-LDC-FI-3O            PIC X(2)   VALUE  '3O'.                  
000164*                                                                         
000165     03  WC-LDC-FR-3P            PIC X(2)   VALUE  '3P'.                  
000166*                                                                         
000167     03  WC-LDC-NL-3R            PIC X(2)   VALUE  '3R'.                  
000168*                                                                         
000169     03  WC-LDC-PL-3S            PIC X(2)   VALUE  '3S'.                  
000170*                                                                         
000171     03  WC-LDC-DE-3T            PIC X(2)   VALUE  '3T'.                  
000172*                                                                         
000173     03  WC-LDC-XX-4A            PIC X(2)   VALUE  '4A'.                  
000174*                                                                         
000175     03  WC-LDC-XX-4B            PIC X(2)   VALUE  '4B'.                  
000176*                                                                         
000177     03  WC-LDC-XX-4C            PIC X(2)   VALUE  '4C'.                  
000178*                                                                         
000179     03  WC-LDC-XX-4D            PIC X(2)   VALUE  '4D'.                  
000180*                                                                         
000181     03  WC-LDC-CN-7A            PIC X(2)   VALUE  '7A'.                  
000182*                                                                         
000183     03  WC-LDC-CN-7B            PIC X(2)   VALUE  '7B'.                  
000184*                                                                         
000185     03  WC-LDC-CN-7C            PIC X(2)   VALUE  '7C'.                  
000186*                                                                         
000187     03  WC-LDC-CN-7D            PIC X(2)   VALUE  '7D'.                  
000188*                                                                         
000189     03  WC-LDC-CN-7E            PIC X(2)   VALUE  '7E'.                  
000190*                                                                         
000191     03  WC-LDC-CN-7F            PIC X(2)   VALUE  '7F'.                  
000192*                                                                         
000193     03  WC-LDC-CN-7G            PIC X(2)   VALUE  '7G'.                  
000194*                                                                         
000195     03  WC-LDC-CN-7H            PIC X(2)   VALUE  '7H'.                  
000196*                                                                         
000197     03  WC-DDC-SE               PIC X(2)   VALUE  'SE'.                  
000198*                                                                         
000199     03  WC-DDC-NO               PIC X(2)   VALUE  'NO'.                  
000200*                                                                         
000201     03  WC-DDC-BE               PIC X(2)   VALUE  'BE'.                  
000202*                                                                         
000203     03  WC-DDC-DE               PIC X(2)   VALUE  'DE'.                  
000204*                                                                         
000205     03  WC-DDC-NL               PIC X(2)   VALUE  'NL'.                  
000206*                                                                         
000207     03  WC-DDC-GB               PIC X(2)   VALUE  'GB'.                  
000208*                                                                         
000209*** END COPY WWDCKONS  LENGTH=176                                         
