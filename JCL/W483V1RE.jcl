//W483V1RE JOB (650W4830100W483V1RE,W100),'RTN W483V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//RENEFR     EXEC W001PTSO                                                      
//SYSTSIN  DD  *                                                                
  %RENDATE  'W479.W483V1.W47953(+0)'  'W020.V%AAVV..EFRRAD'                     
  %RENDATE  'W479.W483V1.W47955(+0)'  'W020.V%AAVV..FAKTRAD'                    
  %RENDATE  'W479.W483V1.W47960(+0)'  'W020.V%AAVV..FKOLLI'                     
  %RENDATE  'W483.W483V1.W48313(+0)'  'W020.V%AAVV..W48313'                     
  %RENDATE  'W483.W483V1.W48314(+0)'  'W020.V%AAVV..W48314'                     
  %RENDATE  'W020.W483V1.W02005(+0)'  'W020.P%AAP..HISTSUM(+1)'                 
  %RENDATE  'W020.W483V1.W02055(+0)'  'W020.R%AARP..HISTSUM(+1)'                
//*                                                                             
//FREE       EXEC WFREE,NAME=W483V1,MAXRC=8                                     
//*                                                                             
//SOP        EXEC WSOPEND,PROCESS=W483V1RE                                      
