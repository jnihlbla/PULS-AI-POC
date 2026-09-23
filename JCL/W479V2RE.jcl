//W479V2RE JOB (650W0010300W479V2RE,W100),'RTN W479V2',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//RENFRAD    EXEC W001PTSO                                                      
//SYSTSIN  DD  *                                                                
  %RENDATE  'WXTR.W479V2.WXTRB0(+0)'  'WXTR.V&YYWW..WXTRB0'                     
  %RENDATE  'W479.W479V2.W47962(+0)'  'W020.V&YYWW..FLKOLLI'                    
//*                                                                             
//FREE    EXEC WFREE,NAME=W479V2,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W479V2RE                                         
