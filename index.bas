    declare Sub main()
	declare sub addkey(names as string,ppar as integer)
	declare sub split()
	declare sub startcode()
	declare function findvar(names as string)as integer
	declare function addvar(names as string,types as integer,line1 as integer)as integer
	declare sub addcode(names as string)
	declare sub addhead(names as string)
	declare sub addbody(names as string)
	declare sub addtail(names as string)
	declare function addlabel(names as string,state as integer,address as integer,definer as integer)as integer
	declare function findlabel(names as string)as integer
	declare function findstate()as integer
	declare function addfor(addresss as integer,forvars as integer,forfroms as integer,forintos as integer,forsteps as integer)as integer
	declare sub clearbody()
	dim shared llline as integer
	dim shared ts as integer
	dim shared sss as string
	dim shared s as string
	dim shared ss as string
	dim shared n as integer
	dim shared c as string
	dim shared ii as integer
	dim shared i as integer
	dim shared length as integer
	dim shared separete(240) as string
	dim shared keycount as  integer
	dim shared varscount as integer
	dim shared keywords(600) as string
	dim shared par(3000) as integer
	dim shared labelss(3000) as string
	dim shared labeladdress(3000) as integer
	dim shared labelstate(3000) as integer
	dim shared labelindex as integer
	dim shared vars(3000) as string
	dim shared errorss as integer
	dim shared errorssi as integer
	dim shared parcount as integer
	dim shared par1 as string
	dim shared vvv as string
	dim shared t1 as string
	dim shared tt1 as string
	dim shared t as string
	dim shared tt as string
	dim shared iii as integer
	dim shared aa as integer
	dim shared aaa as integer
	dim shared bb as integer
	dim shared bbb as integer
	dim shared bbb1 as integer
	dim shared bbb2 as integer
	dim shared bbb3 as integer
	dim shared bbb4 as integer
	dim shared bbb5 as integer
	dim shared bbb6 as integer
	dim shared tc as string
	dim shared tc1 as string
	dim shared tc2 as string
	dim shared tc3 as string
	dim shared tc4 as string
	dim shared tc5 as string
	dim shared tc6 as string
	dim shared forvar(1300) as integer
	dim shared forfrom(1300) as integer
	dim shared forinto(1300) as integer
	dim shared forstep(1300) as integer
	dim shared foraddress(1300) as integer
	dim shared forcount as integer
	dim shared varstype(3000) as integer
	dim shared line11(3000) as integer
	dim shared labeldefined(3000) as integer
	dim shared debug as string
	dim shared rtxt as string
    dim shared fi As long
    dim shared fn As double



main


'----------------------------------------------------------------------------------
    Sub main()
		open command$ for input as #4
		ts=0
		llline=0
		startcode()
		clearbody()
			while(not eof(4))
			line input #4,ss
			ss=trim(ss)
			addtail("@ "+ss)
			addbody("@ "+ss)
			debug=ss
				llline=llline+1
				length=1
				split
				
				errorss=1

				if length>-1 then 
					par1=lcase(trim(separete(0)))

'----------------------------------------------------------------------------------
'key print,var
					if par1=keywords(0) then
						errorssi=0
						if par(0)=length then

							tc=ucase(trim(separete(1)))

							bbb=findvar(tc)
							if bbb<>-1 and tc<>"" then


								if varstype(bbb)<10 then	 


									addtail("	ldr r1,XL"+(trim(str(line11(bbb)+9000))))
									addtail("	mov r0,#3")
									addtail("	ldr r4,var_sys_call")
									addtail("	blx	r4")
									errorssi=-1
									errorss=0

								
								else
									iii=1+iii
									goto errorhandler
								end if
							end if
						end if 
						goto allkey
					end if 
'----------------------------------------------------------------------------------
'key set ,constant,text
					if par1=keywords(1) then 
						errorssi=1

						if par(1)=length then
							tc=ucase(trim(separete(1)))
							if findvar(tc)=-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								addvar(tc,0,iii)							
								addbody("L"+trim(str(iii+9000))+!": .asciz \""+separete(2)+!" \\n\\0\"")
								addbody("XL"+trim(str(iii+9000))+": .word "+"L"+trim(str(iii+9000)))
							else
									iii=1+iii
								goto errorhandler
							end if 
							errorssi=-1
							errorss=0
						end if
						goto allkey
					end if
'----------------------------------------------------------------------------------
'key no line
					if par1=keywords(2) or par1="	" or par1="		" then 
						errorssi=2
						if length=1 then
							errorssi=-1
							errorss=0
						end if
						goto allkey
					end if
'----------------------------------------------------------------------------------
'key echo,text
					if par1=keywords(3) then
						errorssi=3
						if par(3)=length then


									addtail("	ldr r1,XL"+(trim(str(iii+9000))))
									addtail("	mov r0,#3")
									addtail("	ldr r4,var_sys_call")
									addtail("	blx	r4")
								addbody("L"+trim(str(iii+9000))+!": .asciz \""+separete(1)+!"\\r\\n\\0\"")
								addbody("XL"+trim(str(iii+9000))+": .word "+"L"+trim(str(iii+9000)))



						else
							iii=1+iii
							goto errorhandler

						end if 
						errorssi=-1
						errorss=0

						goto allkey
					end if 

'----------------------------------------------------------------------------------
'key wait,var to put key code
					if par1=keywords(4) then
						errorssi=4
						if par(4)=length then

							tc=ucase(trim(separete(1)))

							bbb=findvar(tc)
							if bbb<>-1 and tc<>"" then


								if varstype(bbb)<10 then	 

									addtail("	JMPS"+(trim(str(line11(bbb)+9000)))+":")
									addtail("	mov r0,#18")
									addtail("	ldr r4,var_sys_call")
									addtail("	blx	r4")
									addtail("	mov 	r1,#0")
									addtail("	cmp	r1,r0")
									addtail("	bne	JMPSS"+(trim(str(line11(bbb)+9000))))
									addtail("	b	JMPS"+(trim(str(line11(bbb)+9000))))
									addtail("	JMPSS"+(trim(str(line11(bbb)+9000)))+":")
									addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))

							errorssi=-1
							errorss=0

								 
								else
									iii=1+iii
									goto errorhandler
								end if
							end if
						end if 
						goto allkey
					end if 

'----------------------------------------------------------------------------------
'key integer ,var,number value
					if par1=keywords(5) then 
						errorssi=5

						if par(5)=length then
							tc=ucase(trim(separete(1)))
							if findvar(tc)=-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								addvar(tc,6,iii)
								n=val(trim(separete(2)))
								addbody("L"+trim(str(iii+9000))+": .word "+str(n))
								addbody("XL"+trim(str(iii+9000))+": .word L"+trim(str(iii+9000)))

							else
									iii=1+iii
								goto errorhandler
							end if 
							errorssi=-1
							errorss=0
						end if
						goto allkey
					end if


'----------------------------------------------------------------------------------
'key let,var,value number
					if par1=keywords(6) then
						errorssi=6
						if par(6)=length then

							tc=ucase(trim(separete(1)))

							bbb=findvar(tc)
							if bbb<>-1 and tc<>"" then


								if varstype(bbb)=6 then	 

									n=val(trim(separete(2)))
									addbody("kL"+trim(str(iii+9000))+": .word "+trim(str(n)))
									addbody("XKL"+trim(str(iii+9000))+": .word L"+trim(str(iii+9000)))
									addtail("	ldr r0,kL"+trim(str(iii+9000)))
									addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))
									errorssi=-1
									errorss=0

								else

									if varstype(bbb)=12 then	 
										fn=val(trim(separete(2)))
										fn=fn*100
										fi=fn
										addbody("kL"+trim(str(iii+9000))+": .word "+trim(str(fi)))
										addbody("XKL"+trim(str(iii+9000))+": .word L"+trim(str(iii+9000)))
										addtail("	ldr r0,kL"+trim(str(iii+9000)))
										addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))


										errorssi=-1
										errorss=0

									else

										if varstype(bbb)<5 then
											addvar(tc,0,iii)
											addbody(!"L"+trim(str(iii+9000))+!": .ascii \""+separete(2)+!"\\n\\0\"")
											addbody(!"XL"+trim(str(iii+9000))+!": .word L"+trim(str(iii+9000)))
											addtail("	ldr 	r1,XL"+(trim(str(line11(bbb)+9000))))
											addtail("	ldr 	r2,XL"+(trim(str(iii+9000))))
											addtail("	mov 	r0,#34")
											addtail("	ldr 	r4,var_sys_call")
											addtail("	blx	r4")

											errorssi=-1
											errorss=0
										else


											iii=1+iii
										
											goto errorhandler
										end if
									end if
								end if
							end if
						end if 
						goto allkey
					end if 


'----------------------------------------------------------------------------------
'key add,var3,var1,var2
					if par1=keywords(7) then
						errorssi=7
						if par(7)=length then

							tc=ucase(trim(separete(1)))
							tc1=ucase(trim(separete(2)))
							tc2=ucase(trim(separete(3)))

							bbb=findvar(tc)
							bbb1=findvar(tc1)
							bbb2=findvar(tc2)
							if bbb<>-1 and tc<>"" and bbb1<>-1 and tc1<>"" and bbb2<>-1 and tc2<>"" then


								if varstype(bbb)=6 and varstype(bbb1)=6 and varstype(bbb2)=6 then	 

									addtail("	ldr r1,L"+(trim(str(line11(bbb1)+9000))))
									addtail("	ldr r2,L"+(trim(str(line11(bbb2)+9000))))
									addtail("	mov r0,#21")
									addtail("	ldr r4,var_sys_call")
									addtail("	blx	r4")
									addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))

							errorssi=-1
							errorss=0

								 
								else

									if varstype(bbb)=12 and varstype(bbb1)=12 and varstype(bbb2)=12 then

										addtail("	ldr r1,L"+(trim(str(line11(bbb1)+9000))))
										addtail("	ldr r2,L"+(trim(str(line11(bbb2)+9000))))
										addtail("	mov r0,#21")
										addtail("	ldr r4,var_sys_call")
										addtail("	blx	r4")
										addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))
										errorssi=-1
										errorss=0

								 
									else
										iii=1+iii
										goto errorhandler
									end if								
								end if
							end if
						end if 
						goto allkey
					end if 



'----------------------------------------------------------------------------------
'key sub,var3,var1,var2
					if par1=keywords(8) then
						errorssi=8
						if par(8)=length then

							tc=ucase(trim(separete(1)))
							tc1=ucase(trim(separete(2)))
							tc2=ucase(trim(separete(3)))

							bbb=findvar(tc)
							bbb1=findvar(tc1)
							bbb2=findvar(tc2)
							if bbb<>-1 and tc<>"" and bbb1<>-1 and tc1<>"" and bbb2<>-1 and tc2<>"" then


								if varstype(bbb)=6 and varstype(bbb1)=6 and varstype(bbb2)=6 then	 

									addtail("	ldr r1,L"+(trim(str(line11(bbb1)+9000))))
									addtail("	ldr r2,L"+(trim(str(line11(bbb2)+9000))))
									addtail("	mov r0,#22")
									addtail("	ldr r4,var_sys_call")
									addtail("	blx	r4")
									addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))
								 	errorssi=-1
								errorss=0

								else


									if varstype(bbb)=12 and varstype(bbb1)=12 and varstype(bbb2)=12 then

										addtail("	ldr r1,L"+(trim(str(line11(bbb1)+9000))))
										addtail("	ldr r2,L"+(trim(str(line11(bbb2)+9000))))
										addtail("	mov r0,#22")
										addtail("	ldr r4,var_sys_call")
										addtail("	blx	r4")
										addtail("	str r0,L"+(trim(str(line11(bbb)+9000))))
										errorssi=-1
										errorss=0

									else								 

									iii=1+iii
									goto errorhandler
									end if								
								end if
							end if
						end if 
						goto allkey
					end if 



'----------------------------------------------------------------------------------
'key exit
					if par1=keywords(9) then
						errorssi=9
						if par(9)=length then


								addtail ("	mov 	r1,#1")
								addtail ("	mov 	r0,#0")
								addtail ("	cmp	r1,r0")
								addtail("	bne	exit")


						else
							iii=1+iii
							goto errorhandler

						end if 
						errorssi=-1
						errorss=0

						goto allkey
					end if 








'----------------------------------------------------------------------------------
'key label,label id
					if par1=keywords(10) then 
						errorssi=10

						if par(10)=length then
							tc=ucase(trim(separete(1)))
									
							bbb=findlabel(tc)
							if bbb=-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								addlabel(tc,1,iii,1)
								addtail("LL"+trim(str(iii+8000))+":")
							errorssi=-1
							errorss=0

							else

								if bbb>-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) and labelstate(bbb)=0 then 
									
									labeldefined(bbb)=1
									addtail("LL"+trim(str(labeladdress(bbb)+8000))+":")
									labelstate(bbb)=1
							errorssi=-1
							errorss=0

								else						
									iii=1+iii
									goto errorhandler
								end if
							end if 
						end if
						goto allkey
					end if





'----------------------------------------------------------------------------------
'key goto,label id
					if par1=keywords(11) then 
						errorssi=11

						if par(11)=length then
							tc=ucase(trim(separete(1)))
							bbb=findlabel(tc)
							if bbb=-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								addlabel(tc,0,iii,0)
								addtail ("	mov 	r1,#1")
								addtail ("	mov 	r0,#0")
								addtail ("	cmp	r1,r0")
								addtail("	bne	LL"+trim(str(iii+8000)))
								errorssi=-1
								errorss=0
							errorssi=-1
							errorss=0

							else

								if bbb>-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								addtail ("	mov 	r1,#1")
								addtail ("	mov 	r0,#0")
								addtail ("	cmp	r1,r0")
									addtail("	bne LL"+trim(str(labeladdress(bbb)+8000)))
									errorssi=-1
									errorss=0
							errorssi=-1
							errorss=0


								else						
									iii=1+iii
									goto errorhandler
								end if
							end if 
						end if
						goto allkey
					end if



'----------------------------------------------------------------------------------
'key return
					if par1=keywords(12) then
						errorssi=12
						if par(12)=length then
						addtail("	sub	sp, fp, #4")
							addtail("	pop	{fp, pc}")
						else
							iii=1+iii
							goto errorhandler

						end if 
						errorssi=-1
						errorss=0

						goto allkey
					end if 

'----------------------------------------------------------------------------------
'key gosub,label id
					if par1=keywords(18) then 
						errorssi=18

						if par(18)=length then
							tc=ucase(trim(separete(1)))
							bbb=findlabel(tc)
							if bbb=-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								
								addlabel(tc,0,iii,0)
								addtail("	bl LL"+trim(str(iii+8000)))

								errorssi=-1
								errorss=0
							else

								if bbb>-1 and tc<>"" and (asc(tc)>(asc("A")-1)) and (asc(tc)<(asc("Z")+1)) then 
								
										addtail("	bl LL"+trim(str(iii+8000)))

									errorssi=-1
									errorss=0
								else						
									iii=1+iii
									goto errorhandler
								end if
							end if 

						end if
						goto allkey
					end if

'----------------------------------------------------------------------------------
'key like,var1,var2,goto label id
					if par1=keywords(13) then
						errorssi=13
						if par(13)=length then

							tc=ucase(trim(separete(1)))
							tc1=ucase(trim(separete(2)))
							tc2=ucase(trim(separete(3)))

							bbb=findvar(tc)
							bbb1=findvar(tc1)
							bbb2=findlabel(tc2)
							if bbb<>-1 and tc<>"" and bbb1<>-1 and tc1<>"" and tc2<>"" then


								if varstype(bbb)=6 and varstype(bbb1)=6 then	 



									if bbb2=-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
										addlabel(tc2,0,iii,0)

										addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
										addtail("	ldr r2,L"+(trim(str(line11(bbb1)+9000))))
										addtail("	mov r0,#25")
										addtail("	ldr r4,var_sys_call")
										addtail("	blx	r4")
										addtail ("	mov 	r1,#0")
										addtail ("	cmp	r1,r0")
										addtail("	bne LL"+trim(str(iii+8000)))
										errorssi=-1
										errorss=0

									else
	
										if bbb2>-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
											addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
											addtail("	mov r2,L"+(trim(str(line11(bbb1)+9000))))
											addtail("	mov r0,#25")
											addtail("	ldr r4,var_sys_call")
											addtail("	blx	r4")
											addtail ("	mov 	r1,#0")
											addtail ("	cmp	r1,r0")
											addtail("	bne LL"+trim(str(labeladdress(bbb2)+8000)))
											errorssi=-1
											errorss=0


										else

											iii=1+iii
											goto errorhandler
										end if
										

									end if
								else

									iii=1+iii
									goto errorhandler
								end if
							else

								iii=1+iii
								goto errorhandler

							end if
													end if 
						goto allkey
					end if 


'----------------------------------------------------------------------------------
'key diferent,var1,var2,goto label id
					if par1=keywords(14) then
						errorssi=14
						if par(14)=length then

							tc=ucase(trim(separete(1)))
							tc1=ucase(trim(separete(2)))
							tc2=ucase(trim(separete(3)))

							bbb=findvar(tc)
							bbb1=findvar(tc1)
							bbb2=findlabel(tc2)
							if bbb<>-1 and tc<>"" and bbb1<>-1 and tc1<>"" and tc2<>"" then


								if varstype(bbb)=6 and varstype(bbb1)=6 then	 



									if bbb2=-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
										addlabel(tc2,0,iii,0)

										addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
										addtail("	ldr r2,L"+(trim(str(line11(bbb1)+9000))))
										addtail("	mov r0,#26")
										addtail("	ldr r4,var_sys_call")
										addtail("	blx	r4")
										addtail ("	mov 	r1,#0")
										addtail ("	cmp	r1,r0")
										addtail("	bne LL"+trim(str(iii+8000)))
										errorssi=-1
										errorss=0

									else
	
										if bbb2>-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
											addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
											addtail("	ldr r2,L"+(trim(str(line11(bbb1)+9000))))
											addtail("	mov r0,#25")
											addtail("	ldr r4,var_sys_call")
											addtail("	blx	r4")
											addtail ("	mov 	r1,#0")
											addtail ("	cmp	r1,r0")
											addtail("	bne LL"+trim(str(labeladdress(bbb2)+8000))) 
											errorssi=-1
											errorss=0


										else

											iii=1+iii
											goto errorhandler
										end if
										

									end if
								else

									iii=1+iii
									goto errorhandler
								end if
							else

								iii=1+iii
								goto errorhandler

							end if
													end if 
						goto allkey
					end if 


'----------------------------------------------------------------------------------

'key big,var1,var2,goto label id
					if par1=keywords(15) then
						errorssi=15
						if par(15)=length then

							tc=ucase(trim(separete(1)))
							tc1=ucase(trim(separete(2)))
							tc2=ucase(trim(separete(3)))

							bbb=findvar(tc)
							bbb1=findvar(tc1)
							bbb2=findlabel(tc2)
							if bbb<>-1 and tc<>"" and bbb1<>-1 and tc1<>"" and tc2<>"" then


								if varstype(bbb)=6 and varstype(bbb1)=6 then	 



									if bbb2=-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
										addlabel(tc2,0,iii,0)

										addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
										addtail("	ldr r2,L"+(trim(str(line11(bbb1)+9000))))
										addtail("	mov r0,#27")
										addtail("	ldr r4,var_sys_call")
										addtail("	blx	r4")
										addtail ("	mov 	r1,#0")
										addtail ("	cmp	r1,r0")
										addtail("	bne LL"+trim(str(iii+8000)))
										errorssi=-1
										errorss=0

									else
	
										if bbb2>-1 and tc2<>"" and (asc(tc2)>(asc("A")-1)) and (asc(tc2)<(asc("Z")+1)) then 
											addtail("	ldr r1,L"+(trim(str(line11(bbb)+9000))))
											addtail("	ldr r2,L"+(trim(str(line11(bbb1)+9000))))
											addtail("	mov r0,#27")
											addtail("	ldr r4,var_sys_call")
											addtail("	blx	r4")
											addtail ("	mov 	r1,#0")
											addtail ("	cmp	r1,r0")
											addtail("	bne LL"+trim(str(labeladdress(bbb2)+8000))) 
											errorssi=-1
											errorss=0


										else

											iii=1+iii
											goto errorhandler
										end if
										

									end if
								else

									iii=1+iii
									goto errorhandler
								end if
							else

								iii=1+iii
								goto errorhandler

							end if
													end if 
						goto allkey
					end if 


'----------------------------------------------------------------------------------

'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------
'----------------------------------------------------------------------------------

'----------------------------------------------------------------------------------
'key pusharm
					if par1=keywords(83) then
						errorssi=83
						if par(12)=length then
									addtail("	push	{fp, lr}")
									addtail("	add	fp, sp, #4")
									addtail("	sub	sp, sp, #8")
						else
							iii=1+iii
							goto errorhandler

						end if 
						errorssi=-1
						errorss=0

						goto allkey
					end if 


'----------------------------------------------------------------------------------

'line count
					allkey:
					iii=iii+1


				end if
				if errorssi<>-1 then goto errorhandler
				if errorss<>0 then 
					errorssi=2	
					goto errorhandler
				end if					




			wend
			

' next out of for
				if forcount<>0 then 
					errorssi=30	
					print "error on next";iii
					goto errorhandler
				end if					



'error on label
		bbb=findstate()
		if bbb<>-1 then 
			errorssi=11
			iii=labeladdress(bbb)
			print "error on label";iii;">>";labelss(bbb)
			goto errorhandler
		end if
		close #4
			vvv=tt + t+chr(13)+chr(10)+"endf: .word "
			vvv=tt + t+chr(13)+chr(10)+!"endf: .ascii \"\\r\\n\\0\""
	
	open "out.asm" for output as #5
		print #5,vvv
	close #5
	shell "as -o out.o out.asm "
	shell "objcopy out.o -O binary out.api "
	
	goto gotoexit

	errorhandler:
	print "error on line "+str(iii)+" keyword :"+keywords(errorssi)+">>"+debug
	escapehandler:
	gotoexit:
    End Sub

sub addkey(names as string,ppar as integer)

		keywords(keycount)=names
		par(keycount)=ppar
		keycount=keycount+1

end sub

function findvar(names as string)as integer
	aaa=-1
	if varscount>0 then 
		for aa=0 to varscount-1
			if vars(aa)=names then
				aaa=aa
				goto findvarexit
			end if
		next
	end if 
	findvarexit:
	return aaa
end function

function addvar(names as string,types as integer,line1 as integer)as integer
	vars(varscount)=names
	varstype(varscount)=types
	line11(varscount)=line1
	varscount=varscount+1
	return varscount
end function




sub addcode(names as string)

		t1=t1+names+chr(13)+chr(10)

end sub

sub addhead(names as string)

		tt1=tt1+names+chr(13)+chr(10)

end sub


sub addbody(names as string)

		t=t+names+chr(13)+chr(10)

end sub


sub addtail(names as string)

		tt=tt+names+chr(13)+chr(10)

end sub

function addlabel(names as string,state as integer,address as integer,definer as integer)as integer
	labelss(labelindex)=names
	labeladdress(labelindex)=address
	labelstate(labelindex)=state
	labeldefined(labelindex)=definer
	labelindex=labelindex+1
	return labelindex
end function

function findlabel(names as string)as integer
	aaa=-1
	if labelindex>0 then 
		for aa=0 to labelindex-1
			if labelss(aa)=names then
				aaa=aa
				goto findlabelexit
			end if
		next
	end if 
	findlabelexit:
	return aaa
end function

function findstate()as integer
	aaa=-1
	if labelindex>0 then 
		for aa=0 to labelindex-1
			if labelstate(aa)=0 then
				aaa=aa
				goto findstateexit
			end if
		next
	end if 
	findstateexit:
	return aaa
end function

function addfor(addresss as integer,forvars as integer,forfroms as integer,forintos as integer,forsteps as integer)as integer
	forvar(forcount)=forvars
	foraddress(forcount)=addresss
	forfrom(forcount)=forfroms
	forinto(forcount)=forintos
	forstep(forcount)=forsteps
	forcount=forcount+1
	return forcount
end function



sub clearbody()

		labelindex=0
		varscount=0
		forcount=0
		errorss=0


		t=t1
		tt=tt1

		ts=1
	

	rtxt=""
	ss=""
	s=""

	ss=ss+s+chr(10)

		c=ss



			iii=0
		ts=0



end sub



sub startcode()




'keyword list 
		keycount=0
		addkey ("print",2)
		addkey ("set",3)
		addkey ("",1)
		addkey ("echo",2)
		addkey ("wait",2)
		addkey ("integer",3)
		addkey ("let",3)
		addkey ("add",4)
		addkey ("sub",4)
		addkey ("exit",1)
		addkey ("label",2)
		addkey ("goto",2)
		addkey ("return",1)
		addkey ("like",4)
		addkey ("diferent",4)
		addkey ("big",4)
		addkey ("less",4)
		addkey ("rem",2)
		addkey ("gosub",2)
		addkey ("memfill",4)
		addkey ("memcopy",4)
		addkey ("string",3)
		addkey ("strcat",3)
		addkey ("strcopy",3)
		addkey ("memmove",4)
		addkey ("input",3)
		addkey ("memback",4)
		addkey ("memford",4)
		addkey ("strfrom",4)
		addkey ("for",5)
		addkey ("next",1)
		addkey ("pointer",3)
		addkey ("copy",4)
		addkey ("str",3)
		addkey ("val",3)
		addkey ("getnumber",2)
		addkey ("printnumber",2)
		addkey ("machine",2)
		addkey ("reset",2)
		addkey ("mul",4)
		addkey ("div",4)
		addkey ("move",3)
		addkey ("alocate",3)
		addkey ("call",6)
		addkey ("file.creat",2)
		addkey ("file.open",3)
		addkey ("file.close",2)
		addkey ("file.read",4)
		addkey ("file.write",4)
		addkey ("string.len",3)
		addkey ("timer.sleep",2)
		addkey ("timer.rnd",2)
		addkey ("stack.push",2)
		addkey ("mem.peek",3)
		addkey ("mem.poke",3)
		addkey ("bits.and",4)
		addkey ("bits.not",3)
		addkey ("mem.reserve",3)
		addkey ("far.into",4)
		addkey ("far.from",4)
		addkey ("text",3)
		addkey ("string.comp",4)
		addkey ("string.lower",2)
		addkey ("string.high",2)
		addkey (":",2)
		addkey ("string.findchr",4)
		addkey (";",2)
		addkey ("string.findstr",4)
		addkey ("inkey",2)
		addkey ("const",2)
		addkey ("locate",4)
		addkey ("screen",2)
		addkey ("textout",4)
		addkey ("border",2)
		addkey ("float",3)
		addkey ("back",2)
		addkey ("hline",5)
		addkey ("doevents",1)
		addkey ("box",6)
		addkey ("file.chain",2)
		addkey ("file.exec",2)
		addkey ("timer.cicle",3)'81
		addkey ("ver",1)'82
		addkey ("pusharm",1)'83
		color 15,5

'code tail

			t1=""
			addcode ("")
			addcode ("@end of body")
			addcode ("exit:")
			addcode ("")
			addcode("	mov r0,#19")
			addcode("	ldr r4,var_sys_call")
			addcode("	blx	r4")
			addcode ("	sub	sp, fp, #4")
			addcode ("	@ sp needed")
			addcode ("	pop	{fp, pc}")
			addcode (!"	var_sys_message: .asciz \"ARM build in index developer tools.... \\0\"")
			addcode ("var_sys_call: .word 0")
			addcode ("@start tail")
			addcode ("")

'add head


			tt1=""
			addhead ("	.align	2")
			addhead ("main:")
			addhead ("	b	mains")
			addhead (!"	var_sys_messages: .asciz \"ARM build in index developer tools....... \\0\"")
			addhead ("mains:")
			addhead ("	push	{fp, lr}")
			addhead ("	add	fp, sp, #4")
			addhead ("	sub	sp, sp, #16")
			addhead ("	str	r3,var_sys_call")
			addhead ("		@head")
			addhead ("")
			addhead ("@body start")

end sub

sub split()
	dim hl as integer
	dim bss as string
	dim varc as integer
	dim varcc as integer
	dim varclen as integer
	hl=1
	varcc=1
	varclen=len(ss)
	for varc=1 to varclen
		if(asc(mid(ss,varc,1))>32) then
			ss=mid(ss,varc,varclen-varc+1)
			varc=varclen+2
		end if 
	next varc

	varclen=len(ss)
	for varc=0 to varclen
		if(asc(mid(ss,varclen-varc,1))>32) then
			ss=mid(ss,1,varclen-varc)
			varc=varclen+2
		end if 
	next varc
	length=0
	hl=1	
	while hl <> 0
		hl=instr(ss,",")
		if hl=-1 then
			separete(length)=ss
		else
			separete(length)=trim(mid(ss,1,hl-1))
			ss=mid(ss,hl+1)
		end if
		length=length+1
			
			
	wend
end sub
