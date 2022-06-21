





		mov	sptr,phsa
		getword sptrh,sptr,#1
		getword sptrl,sptr,#0
		rdlut	spl1,sptrh
		add 	sptrh,#1
		and	sptrh,#512
		rdlut	spl2,sptrh
		mov	sptrh,##$80000000
		sub     sptrh,sptrl
		shr	sptrl,#1
		muls 	spl2,sptrl
		shr 	sptrh,#1
		muls 	spl1,sptrh
		adds    spl1,spl2
